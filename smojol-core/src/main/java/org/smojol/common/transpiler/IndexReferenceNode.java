package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

import java.util.List;

public class IndexReferenceNode extends TranspilerNode {
    private final SymbolReferenceNode root;
    private final List<TranspilerNode> indexes;

    public IndexReferenceNode(SymbolReferenceNode root, List<TranspilerNode> indexes) {
        super(ImmutableList.of(SemanticCategory.REFERENCE));
        this.root = root;
        this.indexes = indexes;
    }

    @Override
    public String description() {
        return String.format("index(%s, %s)", root.description(), String.join(", ", indexes.stream().map(TranspilerNode::description).toList()));
    }
}
