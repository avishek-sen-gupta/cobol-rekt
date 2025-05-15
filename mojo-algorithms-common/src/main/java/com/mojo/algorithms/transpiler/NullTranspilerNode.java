package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.SemanticCategory;
import com.mojo.algorithms.domain.TranspilerNode;

public class NullTranspilerNode extends TranspilerNode {
    public NullTranspilerNode() {
        super(ImmutableList.of(SemanticCategory.NULL));
    }

    @Override
    public String description() {
        return "NULL";
    }
}
