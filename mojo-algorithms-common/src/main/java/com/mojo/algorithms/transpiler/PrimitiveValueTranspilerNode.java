package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;

@Getter
public class PrimitiveValueTranspilerNode extends TranspilerNode {
    private final TypedRecord value;

    public PrimitiveValueTranspilerNode(TypedRecord value) {
        super(ImmutableList.of(SemanticCategory.REFERENCE));
        this.value = value;
    }

    @Override
    public String description() {
        return String.format("primitive(%s)", value.value());
    }
}
