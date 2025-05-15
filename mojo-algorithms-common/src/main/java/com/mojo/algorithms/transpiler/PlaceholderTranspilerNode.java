package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

@Getter
public class PlaceholderTranspilerNode extends TranspilerNode {
    private final String s;

    public PlaceholderTranspilerNode(String s) {
        super(ImmutableList.of(SemanticCategory.UNKNOWN));
        this.s = s;
    }

    @Override
    public String description() {
        return "placeholder: " + s;
    }
}
