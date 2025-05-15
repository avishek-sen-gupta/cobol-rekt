package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

import java.util.Collection;

@Getter
public class NestedConditionNode extends TranspilerNode {
    private final TranspilerNode expression;

    public NestedConditionNode(TranspilerNode expression) {
        super(ImmutableList.of(SemanticCategory.RELATIONAL));
        this.expression = expression;
    }

    @Override
    public String description() {
        return String.format("nest(%s)", expression.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(expression);
    }
}
