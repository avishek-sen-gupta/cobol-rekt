package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class NestedNode extends TranspilerNode {
    private final TranspilerNode expression;

    public NestedNode(TranspilerNode expression) {
        this.expression = expression;
    }

    @Override
    public String description() {
        return String.format("nest(%s)", expression.description());
    }
}
