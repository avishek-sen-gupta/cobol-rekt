package org.smojol.common.transpiler;

import lombok.Getter;

public class InstructionLocationNode extends LocationNode {
    @Getter private final TranspilerNode node;

    public InstructionLocationNode(TranspilerNode node) {
        this.node = node;
    }

    @Override
    public String description() {
        return String.format("loc(%s)", node.id());
    }
}
