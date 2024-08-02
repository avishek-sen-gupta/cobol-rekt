package org.smojol.common.vm.expression;

import lombok.Setter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.ConditionalDataStructure;
import org.smojol.common.vm.structure.NullDataStructure;
import org.smojol.common.vm.exception.UnresolvedVariableReferenceException;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.reference.DeepReferenceBuilder;

public class SimpleConditionVisitor extends CobolExpressionVisitor {
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
        ArithmeticExpressionVisitor arithmeticExpressionVisitor = new ArithmeticExpressionVisitor();
        ctx.arithmeticExpression().accept(arithmeticExpressionVisitor);
        lhs = arithmeticExpressionVisitor.getExpression();
        if (lhs.getClass() == VariableExpression.class) {
            CobolParser.QualifiedDataNameContext qualifiedDataNameContext = ((VariableExpression) lhs).getQualifiedDataNameContext();
            CobolReference reference = new DeepReferenceBuilder().getReference(qualifiedDataNameContext, dataRoot);
            String variableName = qualifiedDataNameContext.getText();

            if (reference.resolve().getClass() == NullDataStructure.class) throw new UnresolvedVariableReferenceException(variableName);
            if (reference.resolve().getClass() == ConditionalDataStructure.class) {
                expression = lhs;
                return expression;
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
                expression = new SimpleConditionExpression(mostRecentLhs, relation).standalone();
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
