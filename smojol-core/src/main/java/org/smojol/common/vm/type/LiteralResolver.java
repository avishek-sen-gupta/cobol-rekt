package org.smojol.common.vm.type;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.expression.LiteralVisitor;

public class LiteralResolver {
    public CobolExpression literal(CobolParser.LiteralContext ctx, AbstractCobolType expectedType) {
        LiteralVisitor literalVisitor = new LiteralVisitor(expectedType);
        ctx.accept(literalVisitor);
        return literalVisitor.getExpression();
    }
}
