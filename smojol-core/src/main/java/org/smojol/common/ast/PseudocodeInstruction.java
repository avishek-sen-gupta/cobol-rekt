package org.smojol.common.ast;

public class PseudocodeInstruction {
    private final String description;
    private final FlowNode node;

    public PseudocodeInstruction(String description, FlowNode node) {
        this.description = description;
        this.node = node;
    }

    @Override
    public String toString() {
        return description;
    }
}
