package org.smojol.toolkit.quad;

import org.smojol.common.pseudocode.*;
import org.smojol.toolkit.quad.generators.QuadGeneration;

public class SubtractQuadGeneration extends QuadGeneration {
    public SubtractQuadGeneration(PseudocodeGraph graph, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        super(graph, symbolTable, symbolReferenceBuilder);
    }

    @Override
    public QuadSequence body(PseudocodeInstruction instruction) {
        return new QuadSequence(symbolTable);
    }
}