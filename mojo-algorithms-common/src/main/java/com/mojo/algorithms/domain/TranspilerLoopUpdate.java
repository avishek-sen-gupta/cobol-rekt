package com.mojo.algorithms.domain;

import com.google.common.collect.ImmutableList;

import java.util.Collection;

public class TranspilerLoopUpdate extends TranspilerNode {
    private final TranspilerNode updateExpression;

    public TranspilerLoopUpdate(TranspilerNode updateExpression) {
        super(ImmutableList.of(SemanticCategory.ITERATION, SemanticCategory.DATA_FLOW));
        this.updateExpression = updateExpression;
    }

    @Override
    public String description() {
        return updateExpression.description();
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(updateExpression);
    }
}
