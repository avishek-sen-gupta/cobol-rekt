package org.smojol.common.transpiler;

public class LessThanOrEqualToNode extends TranspilerComparisonOperator {
    LessThanOrEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("leq(%s, %s)", lhs.description(), rhs.description());
    }
}

