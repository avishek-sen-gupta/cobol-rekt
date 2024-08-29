package org.smojol.common.ast;

import org.smojol.common.pseudocode.PseudocodeInstruction;

public class InstructionFlowNode {
    private final PseudocodeInstruction instruction;

    public InstructionFlowNode(PseudocodeInstruction instruction) {
        this.instruction = instruction;
    }
}
