package org.smojol.common.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

import java.util.List;

public class IndexReferenceNode extends TranspilerNode {
    private final SymbolReferenceNode root;
    private final List<TranspilerNode> indexes;

    public IndexReferenceNode(SymbolReferenceNode root, List<TranspilerNode> indexes) {
        this.root = root;
        this.indexes = indexes;
    }

    @Override
    public String description() {
        return String.format("index(%s, %s)", root.description(), String.join(", ", indexes.stream().map(TranspilerNode::description).toList()));
    }
}
