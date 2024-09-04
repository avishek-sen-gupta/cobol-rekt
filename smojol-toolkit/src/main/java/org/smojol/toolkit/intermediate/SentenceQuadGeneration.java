package org.smojol.toolkit.intermediate;

import org.smojol.common.pseudocode.*;
import org.smojol.toolkit.intermediate.generators.QuadGeneration;

public class SentenceQuadGeneration extends QuadGeneration {
    public SentenceQuadGeneration(PseudocodeGraph graph, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        super(graph, symbolTable, symbolReferenceBuilder);
    }

    @Override
    public QuadSequence body(PseudocodeInstruction instruction) {
        return new QuadSequence();
    }
}
