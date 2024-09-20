package org.smojol.toolkit.quad.generators;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.InstructionEdge;
import org.smojol.common.pseudocode.*;

public class GoToQuadGeneration extends QuadGeneration {

    public GoToQuadGeneration(PseudocodeGraph graph, SmojolSymbolTable symbolTable, SymbolReferenceBuilder symbolReferenceBuilder) {
        super(graph, symbolTable, symbolReferenceBuilder);
    }

    @Override
    public QuadSequence body(PseudocodeInstruction instruction) {
        InstructionEdge edge = graph.edge(instruction);
        AddressSymbolReference addressSymbol = new AddressSymbolReference(edge.getTo().id());
        InstructionQuad gotoQuad = new InstructionQuad(addressSymbol, AbstractOperator.JUMP, addressSymbol);
        return new QuadSequence(symbolTable, ImmutableList.of(gotoQuad));
    }
}
