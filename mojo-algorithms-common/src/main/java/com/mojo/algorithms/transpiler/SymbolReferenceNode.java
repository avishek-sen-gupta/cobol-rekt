package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import com.mojo.algorithms.transpiler.SemanticCategory;

@Getter
public class SymbolReferenceNode extends TranspilerNode {
    private final String name;

    public SymbolReferenceNode(String name) {
        super(ImmutableList.of(SemanticCategory.REFERENCE));
        this.name = name;
    }

    @Override
    public String description() {
        return String.format("ref('%s')", name);
    }
}
