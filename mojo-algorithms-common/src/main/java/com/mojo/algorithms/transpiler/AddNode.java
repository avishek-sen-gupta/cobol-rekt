package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;

import java.util.Collection;

@Getter
public class AddNode extends TranspilerNode {
    private final TranspilerNode lhs;
    private final TranspilerNode rhs;

    public AddNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(ImmutableList.of(SemanticCategory.COMPUTATIONAL, SemanticCategory.DATA_FLOW));
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public String description() {
        return String.format("add(%s, %s)", lhs.description(), rhs.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(lhs, rhs);
    }
}
