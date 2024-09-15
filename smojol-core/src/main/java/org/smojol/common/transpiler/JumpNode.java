package org.smojol.common.transpiler;

public class JumpNode extends TranspilerNode {
    private final LocationNode start;
    private final LocationNode end;

    public JumpNode(LocationNode location) {
        this(location, ProgramTerminalLocationNode.END);
    }

    public JumpNode(LocationNode start, LocationNode end) {
        this.start = start;
        this.end = end;
    }

    @Override
    public String description() {
        return String.format("jump(%s)", start.description());
    }
}
