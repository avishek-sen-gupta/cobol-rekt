package org.smojol.common.transpiler;

public class NotNode extends TranspilerNode {
    private final TranspilerNode expression;

    public NotNode(TranspilerNode expression) {
        this.expression = expression;
    }

    @Override
    public String description() {
        return String.format("not(%s)", expression.description());
    }
}
