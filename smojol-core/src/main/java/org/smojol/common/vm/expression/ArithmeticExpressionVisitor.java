package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public class ArithmeticExpressionVisitor extends AntlrCobolExpressionVisitor {
    @Override
    public CobolExpression visitArithmeticExpression(CobolParser.ArithmeticExpressionContext ctx) {
        MultDivsVisitor multDivsVisitor = new MultDivsVisitor();
        ctx.multDivs().accept(multDivsVisitor);
        expression = multDivsVisitor.getExpression();
        if (ctx.plusMinus().isEmpty()) return expression;
        for (CobolParser.PlusMinusContext plusMinus : ctx.plusMinus()) {
            PlusMinusVisitor secondTermVisitor = new PlusMinusVisitor();
            plusMinus.accept(secondTermVisitor);
            expression = ArithmeticExpression.expression(plusMinus.PLUSCHAR() != null ? BinaryMathOperations.ADD : BinaryMathOperations.SUBTRACT, expression, secondTermVisitor.getExpression());
        }
        return expression;
    }
}
