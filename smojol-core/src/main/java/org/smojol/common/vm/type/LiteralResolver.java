package org.smojol.common.vm.type;

import com.mojo.algorithms.transpiler.TypedRecord;
import com.mojo.algorithms.types.AbstractCobolType;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.LiteralVisitor;
import org.smojol.common.vm.expression.PrimitiveCobolExpression;

public class LiteralResolver {
    public CobolExpression literal(CobolParser.LiteralContext ctx, AbstractCobolType expectedType) {
        LiteralVisitor literalVisitor = new LiteralVisitor(expectedType);
        ctx.accept(literalVisitor);
        return literalVisitor.getExpression();
    }

    public CobolExpression literal(CobolParser.IntegerLiteralContext integerLiteralContext, AbstractCobolType expectedType) {
        return new PrimitiveCobolExpression(TypedRecord.typedNumber(Integer.parseInt(integerLiteralContext.getText())));
    }
}
