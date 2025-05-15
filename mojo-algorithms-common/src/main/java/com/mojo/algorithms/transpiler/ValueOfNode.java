package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

import java.util.Collection;

@Getter
public class ValueOfNode extends TranspilerNode {
    private final TranspilerNode expression;

    public ValueOfNode(TranspilerNode expression) {
        super(ImmutableList.of(SemanticCategory.DEREFERENCE));
        this.expression = expression;
    }

    @Override
    public String description() {
        return String.format("value(%s)", expression.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(expression);
    }
}
