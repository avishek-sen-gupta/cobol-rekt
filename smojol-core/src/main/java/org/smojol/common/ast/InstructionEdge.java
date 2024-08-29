package org.smojol.common.ast;

import org.smojol.common.pseudocode.PseudocodeInstruction;

public class InstructionEdge {
    private final PseudocodeInstruction from;
    private final PseudocodeInstruction instruction;
    private final InstructionEdgeType edgeType;

    public InstructionEdge(PseudocodeInstruction from, PseudocodeInstruction to, InstructionEdgeType edgeType) {
        this.from = from;
        instruction = to;
        this.edgeType = edgeType;
    }
}
