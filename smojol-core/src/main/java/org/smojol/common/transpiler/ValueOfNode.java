package org.smojol.common.transpiler;

public class ValueOfNode extends TranspilerNode {
    private final TranspilerNode expression;

    public ValueOfNode(TranspilerNode expression) {
        this.expression = expression;
    }

    @Override
    public String description() {
        return String.format("value(%s)", expression.description());
    }
}
