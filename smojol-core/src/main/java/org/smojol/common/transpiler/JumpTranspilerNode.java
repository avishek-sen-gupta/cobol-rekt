package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;

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
