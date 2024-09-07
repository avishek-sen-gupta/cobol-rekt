package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public class CobolExpressionBuilder {
    public CobolExpression identifier(CobolParser.GeneralIdentifierContext ctx) {
        GeneralIdentifierVisitor identifierVisitor = new GeneralIdentifierVisitor();
        ctx.accept(identifierVisitor);
        return identifierVisitor.getExpression();
    }

    public CobolExpression literal(CobolParser.LiteralContext ctx) {
        LiteralVisitor literalVisitor = new LiteralVisitor();
        ctx.accept(literalVisitor);
        return literalVisitor.getExpression();
    }

    public CobolExpression arithmetic(CobolParser.ArithmeticExpressionContext ctx) {
        ArithmeticExpressionVisitor arithmeticExpressionVisitor = new ArithmeticExpressionVisitor();
        ctx.accept(arithmeticExpressionVisitor);
        return arithmeticExpressionVisitor.getExpression();
    }
}
