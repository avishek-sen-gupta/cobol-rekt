package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;

import java.util.List;

public class ExpressionComparisonVisitor extends AntlrCobolExpressionVisitor {
    @Override
    public CobolExpression visitRelationCombinedComparison(CobolParser.RelationCombinedComparisonContext ctx) {
        ComparisonOperator relationalOperation = RelationalOperations.create(ctx.relationalOperator());
        if (ctx.arithmeticExpression() != null) {
            expression = new RelationExpression(relationalOperation, new CobolExpressionBuilder().arithmetic(ctx.arithmeticExpression()));
        } else if (ctx.multipleArithmeticExpressions() != null) {
            CobolParser.ArithmeticExpressionContext firstTerm = ctx.multipleArithmeticExpressions().arithmeticExpression();
            expression = new CobolExpressionBuilder().arithmetic(firstTerm);

            List<CobolParser.AdditionalArithmeticExpressionContext> additionalExpressions = ctx.multipleArithmeticExpressions().additionalArithmeticExpression();

            for (CobolParser.AdditionalArithmeticExpressionContext c : additionalExpressions) {
                CobolParser.ArithmeticExpressionContext additionalTerm = c.arithmeticExpression();
                CobolExpression arithmeticExpression = new CobolExpressionBuilder().arithmetic(additionalTerm);
                LogicOperation operator = c.AND() != null ? LogicOperation.AND : LogicOperation.OR;
                expression = graft(operator, expression, arithmeticExpression);
            }
            expression = new RelationExpression(relationalOperation, expression);
        }
        return new NestedConditionExpression(expression);
    }
}
