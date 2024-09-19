package org.smojol.common.transpiler;

public class NotEqualToNode extends TranspilerComparisonOperator {
    NotEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("neq(%s, %s)", lhs.description(), rhs.description());
    }
}
