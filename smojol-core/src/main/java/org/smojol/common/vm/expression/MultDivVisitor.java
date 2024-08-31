package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public class MultDivVisitor extends AntlrCobolExpressionVisitor {
    @Override
    public CobolExpression visitMultDiv(CobolParser.MultDivContext ctx) {
        PowersVisitor powersVisitor = new PowersVisitor();
        ctx.powers().accept(powersVisitor);
        expression = powersVisitor.getExpression();
        return expression;
    }
}
