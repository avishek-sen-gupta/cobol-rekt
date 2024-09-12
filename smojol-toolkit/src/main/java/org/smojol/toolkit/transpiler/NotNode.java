package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

import java.util.List;

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
