package org.smojol.toolkit.quad.generators;

import org.smojol.common.pseudocode.*;

public abstract class QuadGeneration {
    protected final PseudocodeGraph graph;
    protected final SmojolSymbolTable symbolTable;
    protected final SymbolReferenceBuilder symbolReferenceBuilder;

    public QuadGeneration(PseudocodeGraph graph, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        this.graph = graph;
        this.symbolTable = symbolTable;
        this.symbolReferenceBuilder = symbolReferenceBuilder;
    }

    public QuadSequence enter(PseudocodeInstruction instruction) {
        return new QuadSequence(symbolTable);
    }

    public abstract QuadSequence body(PseudocodeInstruction instruction);

    public QuadSequence exit(PseudocodeInstruction instruction) {
        return new QuadSequence(symbolTable);
    }
}
