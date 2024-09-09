package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.vm.structure.ConversionStrategy;
import org.smojol.common.vm.type.AbstractCobolType;
import org.smojol.common.vm.type.TypedRecord;

import java.util.logging.Logger;

// TODO: Merge this to use LiteralResolver
public class LiteralVisitor extends AntlrCobolExpressionVisitor {
    private static final Logger LOGGER = Logger.getLogger(LiteralVisitor.class.getName());
    private final AbstractCobolType expectedType;

    public LiteralVisitor(AbstractCobolType expectedType) {
        this.expectedType = expectedType;
    }

    @Override
    public CobolExpression visitLiteral(CobolParser.LiteralContext ctx) {
        if (ctx.numericLiteral() != null) {
            expression = new PrimitiveCobolExpression(TypedRecord.typedNumber(asNumber(ctx.numericLiteral().getText())));
        } else if (ctx.booleanLiteral() != null) {
            expression = new PrimitiveCobolExpression(TypedRecord.typedBoolean(asBoolean(ctx.booleanLiteral().getText())));
        } else if (ctx.charString() != null) {
            expression = new PrimitiveCobolExpression(TypedRecord.typedString(ctx.numericLiteral().getText()));
        } else if (ctx.figurativeConstant() != null) {
            expression = new PrimitiveCobolExpression(typed(new FigurativeConstantMap().map(ctx.figurativeConstant().getText())));
        } else if (ctx.NONNUMERICLITERAL() != null) {
            expression = new PrimitiveCobolExpression(TypedRecord.typedString(ConversionStrategy.asString(ctx.NONNUMERICLITERAL().getText())));
        }
        return expression;
    }

    private TypedRecord typed(String constant) {
        try {
            return expectedType == AbstractCobolType.STRING
                    ? TypedRecord.typedString(constant)
                    : TypedRecord.typedNumber(asNumber(constant));
        } catch (NumberFormatException e) {
            // TODO: This is a hack; find the correct context that will decide what type the literal should be unambiguously converted
            LOGGER.warning(ConsoleColors.red("Could not convert '" + constant + "' to a number, returning the original string..."));
            return TypedRecord.typedString(constant);
        }
    }

    private Double asNumber(String text) {
        return Double.valueOf(text);
    }

    private Boolean asBoolean(String text) {
        return Boolean.valueOf(text);
    }
}
