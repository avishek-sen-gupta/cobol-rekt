package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class NegativeNode implements TranspilerNode {
    private final TranspilerNode expression;

    public NegativeNode(TranspilerNode expression) {
        this.expression = expression;
    }
}
