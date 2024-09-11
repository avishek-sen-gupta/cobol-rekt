package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class SubtractNode implements TranspilerNode {
    private final TranspilerNode minuend;
    private final TranspilerNode subtrahend;

    public SubtractNode(TranspilerNode minuend, TranspilerNode subtrahend) {
        this.minuend = minuend;
        this.subtrahend = subtrahend;
    }
}
