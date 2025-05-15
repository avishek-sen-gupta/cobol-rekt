package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

@Getter
public class JumpIfTranspilerNode extends TranspilerNode {
    private final LocationNode destination;
    private final TranspilerNode condition;

    public JumpIfTranspilerNode(LocationNode location, TranspilerNode condition) {
        super(ImmutableList.of(SemanticCategory.CONTROL_FLOW));
        this.destination = location;
        this.condition = condition;
    }

    @Override
    public String description() {
        return String.format("jump_if(%s, %s)", condition.description(), destination.description());
    }
}
