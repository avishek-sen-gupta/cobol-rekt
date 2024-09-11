package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

import java.util.List;

public class IndexReferenceNode implements TranspilerNode {
    private final SymbolReferenceNode root;
    private final List<TranspilerNode> indexes;

    public IndexReferenceNode(SymbolReferenceNode root, List<TranspilerNode> indexes) {
        this.root = root;
        this.indexes = indexes;
    }
}
