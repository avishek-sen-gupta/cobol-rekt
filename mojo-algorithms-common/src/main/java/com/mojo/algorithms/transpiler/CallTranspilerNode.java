package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

import java.util.Collection;

@Getter
public class CallTranspilerNode extends TranspilerNode {
    private final TranspilerNode callExpression;

    public CallTranspilerNode(TranspilerNode callExpression) {
        super(ImmutableList.of(SemanticCategory.CONTROL_FLOW));
        this.callExpression = callExpression;
    }

    @Override
    public String description() {
        return String.format("ext_call(%s)", callExpression.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(callExpression);
    }
}
