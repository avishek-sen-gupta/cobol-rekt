package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

import java.util.List;

public class ExpressionComparisonVisitor extends CobolExpressionVisitor {
    @Override
    public CobolExpression visitRelationCombinedComparison(CobolParser.RelationCombinedComparisonContext ctx) {
        ComparisonOperator relationalOperation = RelationalOperations.create(ctx.relationalOperator());
        if (ctx.arithmeticExpression() != null) {
            ArithmeticExpressionVisitor arithmeticExpressionVisitor = new ArithmeticExpressionVisitor();
            ctx.arithmeticExpression().accept(arithmeticExpressionVisitor);
            expression = new RelationExpression(relationalOperation, arithmeticExpressionVisitor.getExpression());
        } else if (ctx.multipleArithmeticExpressions() != null) {
            CobolParser.ArithmeticExpressionContext firstTerm = ctx.multipleArithmeticExpressions().arithmeticExpression();
            ArithmeticExpressionVisitor firstTermVisitor = new ArithmeticExpressionVisitor();
            firstTerm.accept(firstTermVisitor);
            expression = firstTermVisitor.getExpression();

            List<CobolParser.AdditionalArithmeticExpressionContext> additionalExpressions = ctx.multipleArithmeticExpressions().additionalArithmeticExpression();

            for (CobolParser.AdditionalArithmeticExpressionContext c : additionalExpressions) {
                CobolParser.ArithmeticExpressionContext additionalTerm = c.arithmeticExpression();

                ArithmeticExpressionVisitor additionalTermVisitor = new ArithmeticExpressionVisitor();
                additionalTerm.accept(additionalTermVisitor);

                LogicOperation operator = c.AND() != null ? LogicOperation.AND : LogicOperation.OR;
                expression = graft(operator, expression, additionalTermVisitor.getExpression());
            }
            expression = new RelationExpression(relationalOperation, expression);
        }
        return new NestedCondition(expression);
    }
}
