package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.transpiler.SemanticCategory;

public class NotTranspilerNode extends TranspilerNode {
    private final TranspilerNode expression;

    public NotTranspilerNode(TranspilerNode expression) {
        super(ImmutableList.of(SemanticCategory.RELATIONAL));
        this.expression = expression;
    }

    @Override
    public String description() {
        return String.format("not(%s)", expression.description());
    }
}
