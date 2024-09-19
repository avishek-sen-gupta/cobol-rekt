package org.smojol.common.transpiler;

public class EqualToNode extends TranspilerComparisonOperator {
    public EqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("eq(%s, %s)", lhs.description(), rhs.description());
    }
}
