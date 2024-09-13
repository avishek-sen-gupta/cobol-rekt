package org.smojol.common.transpiler;

public class MultiplyNode extends TranspilerNode {
    private final TranspilerNode lhs;
    private final TranspilerNode rhs;

    public MultiplyNode(TranspilerNode lhs, TranspilerNode rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public String description() {
        return String.format("multiply(%s, %s)", lhs.description(), rhs.description());
    }
}
