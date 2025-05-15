package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

import java.util.Collection;

@Getter
public class JumpTranspilerNode extends TranspilerNode {
    private final LocationNode start;
    private final LocationNode end;

    public JumpTranspilerNode(LocationNode location) {
        this(location, LocationNode.NULL);
    }

    public JumpTranspilerNode(LocationNode start, LocationNode end) {
        super(ImmutableList.of(SemanticCategory.CONTROL_FLOW));
        this.start = start;
        this.end = end;
    }

    @Override
    public String description() {
        return String.format("jump(%s, %s)", start.description(), end.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(start, end);
    }
}
