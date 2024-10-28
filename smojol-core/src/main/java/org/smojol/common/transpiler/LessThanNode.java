package org.smojol.common.transpiler;

public class LessThanNode extends TranspilerComparisonOperator {
    public LessThanNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("lt(%s, %s)", lhs.description(), rhs.description());
    }
}
