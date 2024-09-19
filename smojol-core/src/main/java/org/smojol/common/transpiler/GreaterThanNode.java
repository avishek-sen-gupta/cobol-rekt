package org.smojol.common.transpiler;

public class GreaterThanNode extends TranspilerComparisonOperator {
    GreaterThanNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("gt(%s, %s)", lhs.description(), rhs.description());
    }
}
