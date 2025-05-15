package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

import java.util.Collection;

@Getter
public class NegativeNode extends TranspilerNode {
    private final TranspilerNode expression;

    public NegativeNode(TranspilerNode expression) {
        super(ImmutableList.of(SemanticCategory.COMPUTATIONAL));
        this.expression = expression;
    }

    @Override
    public String description() {
        return String.format("negative(%s)", expression.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(expression);
    }
}
