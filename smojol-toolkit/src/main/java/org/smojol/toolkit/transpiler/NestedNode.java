package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class NestedNode implements TranspilerNode {
    private final TranspilerNode expression;

    public NestedNode(TranspilerNode expression) {
        this.expression = expression;
    }
}
