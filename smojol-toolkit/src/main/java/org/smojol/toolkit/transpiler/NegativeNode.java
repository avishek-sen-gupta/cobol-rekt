package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

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
