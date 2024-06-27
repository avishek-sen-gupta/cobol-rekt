package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public class BasisVisitor extends CobolExpressionVisitor {
    @Override
    public CobolExpression visitBasis(CobolParser.BasisContext ctx) {
        if (ctx.generalIdentifier() != null) {
            GeneralIdentifierVisitor identifierVisitor = new GeneralIdentifierVisitor();
            ctx.generalIdentifier().accept(identifierVisitor);
            expression = identifierVisitor.getExpression();
        } else if (ctx.arithmeticExpression() != null) {
            ArithmeticExpressionVisitor arithmeticExpressionVisitor = new ArithmeticExpressionVisitor();
            ctx.arithmeticExpression().accept(arithmeticExpressionVisitor);
            expression = arithmeticExpressionVisitor.getExpression();
        } else if (ctx.literal() != null) {
            LiteralVisitor literalVisitor = new LiteralVisitor();
            ctx.literal().accept(literalVisitor);
            expression = literalVisitor.getExpression();
        }

        return expression;
    }
}
