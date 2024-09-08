package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.ConversionStrategy;
import org.smojol.common.vm.type.CobolDataType;
import org.smojol.common.vm.type.LiteralResolver;
import org.smojol.common.vm.type.TypedRecord;

// TODO: Merge this to use LiteralResolver
public class LiteralVisitor extends AntlrCobolExpressionVisitor {
    private final CobolDataType expectedType;

    public LiteralVisitor(CobolDataType expectedType) {
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
        return expectedType == CobolDataType.STRING
                ? TypedRecord.typedString(constant)
                : TypedRecord.typedNumber(asNumber(constant));
    }

    private Double asNumber(String text) {
        return Double.valueOf(text);
    }

    private Boolean asBoolean(String text) {
        return Boolean.valueOf(text);
    }
}
