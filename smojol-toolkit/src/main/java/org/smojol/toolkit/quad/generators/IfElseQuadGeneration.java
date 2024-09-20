package org.smojol.toolkit.quad.generators;

import org.smojol.common.pseudocode.*;

public class IfElseQuadGeneration extends QuadGeneration {
    public IfElseQuadGeneration(PseudocodeGraph graph, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        super(graph, symbolTable, symbolReferenceBuilder);
    }

    @Override
    public QuadSequence body(PseudocodeInstruction instruction) {
        return new QuadSequence(symbolTable);
    }
}
