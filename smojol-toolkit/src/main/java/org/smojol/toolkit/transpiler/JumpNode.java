package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class JumpNode extends TranspilerNode {
    private final LocationNode location;

    public JumpNode(LocationNode location) {
        this.location = location;
    }

    @Override
    public String description() {
        return String.format("jump(%s)", location.description());
    }
}
