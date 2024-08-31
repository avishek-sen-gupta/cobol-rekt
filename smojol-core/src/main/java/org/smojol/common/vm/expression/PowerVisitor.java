package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public class PowerVisitor extends AntlrCobolExpressionVisitor {
    @Override
    public CobolExpression visitPower(CobolParser.PowerContext ctx) {
        BasisVisitor basisVisitor = new BasisVisitor();
        ctx.basis().accept(basisVisitor);
        expression = basisVisitor.getExpression();
        return expression;
    }
}
