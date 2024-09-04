package org.smojol.toolkit.intermediate.generators;

import org.smojol.common.pseudocode.*;

public class IfQuadGeneration extends QuadGeneration {
    public IfQuadGeneration(PseudocodeGraph graph, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        super(graph, symbolTable, symbolReferenceBuilder);
    }

    @Override
    public QuadSequence body(PseudocodeInstruction instruction) {
        return new QuadSequence();
    }
}
