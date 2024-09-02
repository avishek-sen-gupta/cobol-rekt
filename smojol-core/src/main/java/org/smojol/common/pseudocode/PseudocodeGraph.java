package org.smojol.common.pseudocode;

public record PseudocodeGraph(java.util.List<PseudocodeInstruction> instructions,
                              java.util.List<org.smojol.common.ast.InstructionEdge> edges) {
}
