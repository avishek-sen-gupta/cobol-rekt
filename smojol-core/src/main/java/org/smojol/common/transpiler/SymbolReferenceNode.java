package org.smojol.common.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class SymbolReferenceNode extends TranspilerNode {
    private final String name;

    public SymbolReferenceNode(String name) {
        this.name = name;
    }

    @Override
    public String description() {
        return String.format("ref('%s')", name);
    }
}
