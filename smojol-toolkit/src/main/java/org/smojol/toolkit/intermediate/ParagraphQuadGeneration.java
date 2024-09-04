package org.smojol.toolkit.intermediate;

import org.smojol.common.pseudocode.*;
import org.smojol.toolkit.intermediate.generators.QuadGeneration;

public class ParagraphQuadGeneration extends QuadGeneration {
    public ParagraphQuadGeneration(PseudocodeGraph graph, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        super(graph, symbolTable, symbolReferenceBuilder);
    }

    @Override
    public QuadSequence body(PseudocodeInstruction instruction) {
        return new QuadSequence(symbolTable);
    }
}
