package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class SubtractNode implements TranspilerNode {
    private final TranspilerNode lhs;
    private final TranspilerNode rhs;

    public SubtractNode(TranspilerNode lhs, TranspilerNode rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }
}
