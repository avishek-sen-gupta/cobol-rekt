package org.smojol.common.pseudocode;

import org.smojol.common.ast.InstructionEdge;

public record PseudocodeGraph(java.util.List<PseudocodeInstruction> instructions,
                              java.util.List<org.smojol.common.ast.InstructionEdge> edges) {
    public InstructionEdge edge(PseudocodeInstruction from) {
        return edges.stream().filter(e -> e.getFrom() == from).findFirst().orElse(InstructionEdge.NULL);
    }
}
