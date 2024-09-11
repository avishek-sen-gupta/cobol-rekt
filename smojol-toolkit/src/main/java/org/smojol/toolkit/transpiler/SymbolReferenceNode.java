package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class SymbolReferenceNode implements TranspilerNode {
    private final String name;

    public SymbolReferenceNode(String name) {
        this.name = name;
    }
}
