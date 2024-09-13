package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class OrNode extends TranspilerNode {
    private final TranspilerNode lhs;
    private final TranspilerNode rhs;

    public OrNode(TranspilerNode lhs, TranspilerNode rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public String description() {
        return String.format("or(%s, %s)", lhs.description(), rhs.description());
    }
}