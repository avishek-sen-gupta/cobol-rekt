package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class SetTranspilerNode extends TranspilerNode {
    private final TranspilerNode source;
    private final TranspilerNode destination;

    public SetTranspilerNode(TranspilerNode source, TranspilerNode destination) {
        this.source = source;
        this.destination = destination;
    }

    @Override
    public String description() {
        return String.format("set(%s, %s)", destination.description(), source.description());
    }
}
