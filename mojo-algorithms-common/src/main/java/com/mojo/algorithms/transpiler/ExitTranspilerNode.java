package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.SemanticCategory;
import com.mojo.algorithms.domain.TranspilerNode;

// TODO: This will probably go away
public class ExitTranspilerNode extends TranspilerNode {
    public ExitTranspilerNode() {
        super(ImmutableList.of(SemanticCategory.TERMINAL));
    }

    @Override
    public String description() {
        return "[EXIT]";
    }
}
