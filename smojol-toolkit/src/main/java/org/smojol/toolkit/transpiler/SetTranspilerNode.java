package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class SetTranspilerNode implements TranspilerNode {
    private final TranspilerNode from;
    private final TranspilerNode to;

    public SetTranspilerNode(TranspilerNode from, TranspilerNode to) {
        this.from = from;
        this.to = to;
    }
}
