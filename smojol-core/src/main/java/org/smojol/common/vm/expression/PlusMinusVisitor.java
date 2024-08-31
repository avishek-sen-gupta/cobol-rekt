package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public class PlusMinusVisitor extends AntlrCobolExpressionVisitor {
    @Override
    public CobolExpression visitPlusMinus(CobolParser.PlusMinusContext ctx) {
        MultDivsVisitor multDivsVisitor = new MultDivsVisitor();
        ctx.multDivs().accept(multDivsVisitor);
        expression = multDivsVisitor.getExpression();
        return expression;
    }
}
