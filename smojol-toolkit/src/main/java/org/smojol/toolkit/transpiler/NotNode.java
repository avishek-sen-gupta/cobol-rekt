package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

import java.util.List;

public class NotNode implements TranspilerNode {
    private final TranspilerNode expression;

    public NotNode(TranspilerNode expression) {
        this.expression = expression;
    }
}
