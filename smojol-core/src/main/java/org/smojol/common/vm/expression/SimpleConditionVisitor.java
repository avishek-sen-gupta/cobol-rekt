package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Setter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConditionalDataStructure;
import org.smojol.common.vm.structure.NullDataStructure;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.reference.CobolReferenceBuilder;

public class SimpleConditionVisitor extends AntlrCobolExpressionVisitor {
    private final CobolExpression mostRecentLhs;
    private final ComparisonOperator mostRecentRelationalOperation;
    private final CobolDataStructure dataRoot;
    private CobolExpression lhs;
    @Setter
    private CobolExpression comparison;

    public SimpleConditionVisitor(CobolExpression mostRecentLhs, ComparisonOperator mostRecentRelationalOperation, CobolDataStructure dataRoot) {
        this.mostRecentLhs = mostRecentLhs;
        this.mostRecentRelationalOperation = mostRecentRelationalOperation;
        this.dataRoot = dataRoot;
    }

    @Override
    public CobolExpression visitSimpleCondition(CobolParser.SimpleConditionContext ctx) {
        lhs = new CobolExpressionBuilder().arithmetic(ctx.arithmeticExpression());
        if (lhs instanceof VariableExpression varExpr) {
            CobolReference reference = new CobolReferenceBuilder().getReference(varExpr.getName(), dataRoot);
            CobolDataStructure resolved = reference.resolve();
            if (resolved.getClass() == NullDataStructure.class) {
                // TODO: Temp fix for variables which don't directly appear in data structures like indexes in INDEXED BY clauses
                expression = new PrimitiveCobolExpression(resolved.getValue());
                return expression;
            } else if (resolved instanceof ConditionalDataStructure cds) {
                // TODO: This can be more than just equality
                expression = new SimpleConditionExpression(new VariableExpression(cds.parent().name()), new RelationExpression(RelationalOperation.EQUAL, new VariableExpression(cds.name())));
                expression = new FunctionCallExpression("isInRange", ImmutableList.of(new VariableExpression(cds.parent().name()), new VariableExpression(cds.name())));
                return expression;
//                cds.parent().
//                expression = lhs;
//                return expression;
            }
        }
        if (ctx.fixedComparison() != null) {
            expression = new ClassConditionBuilder().build(ctx.fixedComparison(), lhs);
            return expression;
        }
        if (ctx.fixedComparison() == null && ctx.relationCombinedComparison() == null) {
            // TODO: Distinguish between abbreviated condition and level 88 conditions
            RelationExpression relation = new RelationExpression(mostRecentRelationalOperation, lhs);
            if (mostRecentLhs == null) {
                // Level 88 condition
                expression = new SimpleConditionExpression(lhs).standalone();
            } else {
                // Abbreviated condition
                expression = new SimpleConditionExpression(mostRecentLhs, relation);
            }
            return expression;
        }
        ExpressionComparisonVisitor comparisonVisitor = new ExpressionComparisonVisitor();
        ctx.relationCombinedComparison().accept(comparisonVisitor);
        comparison = comparisonVisitor.getExpression();

        // TODO: fixedComparison is not yet implemented
        expression = new SimpleConditionExpression(lhs, comparison);
        return expression;
    }
}
