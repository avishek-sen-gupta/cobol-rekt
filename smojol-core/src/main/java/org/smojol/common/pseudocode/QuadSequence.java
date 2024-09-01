package org.smojol.common.pseudocode;

import org.smojol.common.vm.expression.CobolExpression;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class QuadSequence {
    private final List<InstructionQuad> quads = new ArrayList<>();
    private final SmojolSymbolTable symbolTable;

    public QuadSequence(SmojolSymbolTable symbolTable) {
        this.symbolTable = symbolTable;
    }

    public SymbolReference existingSymbol(CobolExpression expr) {

        Optional<InstructionQuad> any = quads.stream().filter(q -> q.result().equals(variableSymbolReferenceSearchSpec(expr))).findAny();
        if (any.isEmpty()) throw new UnresolvedSymbolReferenceException(expr);
        return any.get().result();
    }

    private IntermediateSymbolReference variableSymbolReferenceSearchSpec(CobolExpression expression) {
        return new IntermediateSymbolReference(expression, "-1");
    }

    public void add(InstructionQuad quad) {
        quads.add(quad);
    }

    public SymbolReference lastResult() {
        return quads.getLast().result();
    }
}
