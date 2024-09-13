package org.smojol.common.transpiler;

public class NegativeNode extends TranspilerNode {
    private final TranspilerNode expression;

    public NegativeNode(TranspilerNode expression) {
        this.expression = expression;
    }

    @Override
    public String description() {
        return String.format("negative(%s)", expression.description());
    }
}
