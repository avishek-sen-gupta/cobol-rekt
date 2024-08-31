package org.smojol.common.vm.expression;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;

public class AdditionalConditionVisitor extends AntlrCobolExpressionVisitor {
    private final ComparisonOperator mostRecentRelationalOperation;
    private final CobolDataStructure dataRoot;
    private LogicOperation logicOperation;
//    @Getter private ComparisonOperator comparisonOperator;
    private final CobolExpression mostRecentLhs;
//    private ComparisonOperator actualRelationalOperation;
//    private CobolExpression actualLhs;

    public AdditionalConditionVisitor(CobolExpression mostRecentLhs, ComparisonOperator mostRecentRelationalOperation, CobolDataStructure dataRoot) {
        this.mostRecentLhs = mostRecentLhs;
        this.mostRecentRelationalOperation = mostRecentRelationalOperation;
        this.dataRoot = dataRoot;
    }

    @Override
    public CobolExpression visitAdditionalCondition(CobolParser.AdditionalConditionContext ctx) {
        logicOperation = ctx.AND() != null ? LogicOperation.AND : LogicOperation.OR;
        if (ctx.simpleCondition() != null) {
            SimpleConditionVisitor simpleConditionVisitor = new SimpleConditionVisitor(mostRecentLhs, mostRecentRelationalOperation, dataRoot);
            ctx.simpleCondition().accept(simpleConditionVisitor);
            expression = simpleConditionVisitor.getExpression();
        } else if (ctx.nestedCondition() != null) {
            NestedConditionVisitor nestedConditionVisitor = new NestedConditionVisitor(dataRoot);
            ctx.nestedCondition().accept(nestedConditionVisitor);
            expression = nestedConditionVisitor.getExpression();
        } else if (ctx.relationCombinedComparison() != null) {
            ExpressionComparisonVisitor comparisonVisitor = new ExpressionComparisonVisitor();
            ctx.relationCombinedComparison().accept(comparisonVisitor);
            // TODO: Distinguish between abbreviated condition and level 88 conditions
            expression = new SimpleConditionExpression(mostRecentLhs, comparisonVisitor.getExpression());
        }

        if (ctx.NOT() != null)
            expression = new NotExpression(expression);

        return expression;
    }

    public LogicOperation operator() {
        return logicOperation;
    }

    public ComparisonOperator getRelationalOperation() {
        return ((SimpleConditionExpression) expression).getComparison().getRelationalOperation();
    }

    public CobolExpression getLhs() {
        return ((SimpleConditionExpression) expression).getLhs();
    }
}
