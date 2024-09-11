package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class AndNode implements TranspilerNode {
    private final TranspilerNode lhs;
    private final TranspilerNode rhs;

    public AndNode(TranspilerNode lhs, TranspilerNode rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }
}
