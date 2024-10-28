package org.smojol.common.transpiler;

public class GreaterThanOrEqualToNode extends TranspilerComparisonOperator {
    public GreaterThanOrEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("geq(%s, %s)", lhs.description(), rhs.description());
    }
}
