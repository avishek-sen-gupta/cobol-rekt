package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

@Getter
public class SubtractNode extends TranspilerNode {
    private final TranspilerNode minuend;
    private final TranspilerNode subtrahend;

    public SubtractNode(TranspilerNode minuend, TranspilerNode subtrahend) {
        super(ImmutableList.of(SemanticCategory.COMPUTATIONAL, SemanticCategory.DATA_FLOW));
        this.minuend = minuend;
        this.subtrahend = subtrahend;
    }

    @Override
    public String description() {
        return String.format("subtract(%s, %s)", minuend.description(), subtrahend.description());
    }
}
