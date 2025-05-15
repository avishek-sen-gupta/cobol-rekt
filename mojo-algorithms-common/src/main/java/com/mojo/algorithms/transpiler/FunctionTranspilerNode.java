package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.SemanticCategory;
import com.mojo.algorithms.domain.TranspilerNode;

public class FunctionTranspilerNode extends TranspilerNode {
    private final String name;
    private final TranspilerCodeBlockNode body;

    public FunctionTranspilerNode(String name, TranspilerCodeBlockNode body) {
        super(ImmutableList.of(body), ImmutableList.of(SemanticCategory.FUNCTION));
        this.name = name;
        this.body = body;
    }
    @Override
    public String description() {
        return String.format("function %s()\n%s", name, body.description());
    }
}
