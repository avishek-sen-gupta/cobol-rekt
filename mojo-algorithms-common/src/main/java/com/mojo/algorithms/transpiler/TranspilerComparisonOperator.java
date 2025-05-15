package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.SemanticCategory;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;

import java.util.Collection;

@Getter
public abstract class TranspilerComparisonOperator extends TranspilerNode {
    protected final TranspilerNode lhs;
    protected final TranspilerNode rhs;

    public TranspilerComparisonOperator(TranspilerNode lhs, TranspilerNode rhs) {
        super(ImmutableList.of(SemanticCategory.COMPARISON));
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(lhs, rhs);
    }
}
