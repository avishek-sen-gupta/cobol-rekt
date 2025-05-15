package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

@Getter
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

    @Override
    public Collection<TranspilerNode> internalElements() {
        List<TranspilerNode> internals = new ArrayList<>();
        internals.add(root);
        internals.addAll(indexes);
        return internals;
    }
}
