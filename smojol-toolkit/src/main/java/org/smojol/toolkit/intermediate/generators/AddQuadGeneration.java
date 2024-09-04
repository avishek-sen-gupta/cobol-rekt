package org.smojol.toolkit.intermediate.generators;

import org.smojol.common.pseudocode.*;
import org.smojol.toolkit.ast.AddFlowNode;

public class AddQuadGeneration extends QuadGeneration {
    public AddQuadGeneration(PseudocodeGraph graph, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        super(graph, symbolTable, symbolReferenceBuilder);
    }

    @Override
    public QuadSequence body(PseudocodeInstruction instruction) {
        return new QuadSequence();
    }
}
