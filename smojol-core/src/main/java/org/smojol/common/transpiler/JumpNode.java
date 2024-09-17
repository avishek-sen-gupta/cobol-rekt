package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

public class JumpNode extends TranspilerNode {
    private final LocationNode start;
    private final LocationNode end;

    public JumpNode(LocationNode location) {
        this(location, ProgramTerminalLocationNode.END);
    }

    public JumpNode(LocationNode start, LocationNode end) {
        super(ImmutableList.of(SemanticCategory.CONTROL_FLOW));
        this.start = start;
        this.end = end;
    }

    @Override
    public String description() {
        return String.format("jump(%s)", start.description());
    }
}
