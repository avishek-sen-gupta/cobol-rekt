package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;
import org.smojol.common.vm.expression.ComparisonOperator;
import org.smojol.common.vm.expression.RelationalOperation;

public abstract class TranspilerComparisonOperator extends TranspilerNode {
    protected final TranspilerNode lhs;
    protected final TranspilerNode rhs;

    public TranspilerComparisonOperator(TranspilerNode lhs, TranspilerNode rhs) {
        super(ImmutableList.of(SemanticCategory.COMPARISON));
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public static TranspilerComparisonOperator operator(ComparisonOperator comparison, TranspilerNode lhs, TranspilerNode rhs) {
        if (comparison == RelationalOperation.EQUAL) return new EqualToNode(lhs, rhs);
        else if (comparison == RelationalOperation.NOT_EQUAL) return new NotEqualToNode(lhs, rhs);
        else if (comparison == RelationalOperation.GREATER_THAN) return new GreaterThanNode(lhs, rhs);
        else if (comparison == RelationalOperation.GREATER_THAN_OR_EQUAL) return new GreaterThanOrEqualToNode(lhs, rhs);
        else if (comparison == RelationalOperation.LESS_THAN) return new LessThanNode(lhs, rhs);
        else if (comparison == RelationalOperation.LESS_THAN_OR_EQUAL) return new LessThanOrEqualToNode(lhs, rhs);

        throw new UnsupportedOperationException("This is not a valid comparison operator: " + comparison);
    }
}
