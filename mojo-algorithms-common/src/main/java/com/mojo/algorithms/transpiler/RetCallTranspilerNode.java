package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.SemanticCategory;
import com.mojo.algorithms.domain.TranspilerNode;

public class RetCallTranspilerNode extends TranspilerNode {
    private final String fallthroughTarget;

    public RetCallTranspilerNode(String fallthroughTarget) {
        super(ImmutableList.of(), ImmutableList.of(SemanticCategory.BLOCK_BOUNDARY));
        this.fallthroughTarget = fallthroughTarget;
    }

    @Override
    public String description() {
        return String.format("callret (\"%s\")", fallthroughTarget);
    }
}
