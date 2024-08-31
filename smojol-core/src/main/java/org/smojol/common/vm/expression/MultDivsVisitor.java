package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

public class MultDivsVisitor extends AntlrCobolExpressionVisitor {
    @Override
    public CobolExpression visitMultDivs(CobolParser.MultDivsContext ctx) {
        CobolParser.PowersContext firstTerm = ctx.powers();
        PowersVisitor powersVisitor = new PowersVisitor();
        ctx.powers().accept(powersVisitor);
        expression = ArithmeticExpression.expression(firstTerm.MINUSCHAR() != null ? UnaryMathOperations.NEGATIVE : UnaryMathOperations.POSITIVE, powersVisitor.getExpression());
        if (ctx.multDiv().isEmpty()) return expression;
        for (CobolParser.MultDivContext m : ctx.multDiv()) {
            MultDivVisitor secondTermVisitor = new MultDivVisitor();
            m.accept(secondTermVisitor);
            expression = ArithmeticExpression.expression(m.ASTERISKCHAR() != null ? BinaryMathOperations.MULTIPLY : BinaryMathOperations.DIVIDE, expression, secondTermVisitor.getExpression());
        }
        return expression;
    }
}
