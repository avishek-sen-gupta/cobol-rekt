package org.smojol.common.pseudocode;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.vm.expression.CobolExpression;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class QuadSequence {
    @Getter
    private final List<InstructionQuad> quads = new ArrayList<>();
    private final SmojolSymbolTable symbolTable;

    public QuadSequence(SmojolSymbolTable symbolTable) {
        this(symbolTable, ImmutableList.of());
    }

    public QuadSequence(SmojolSymbolTable symbolTable, List<InstructionQuad> instructions) {
        this.symbolTable = symbolTable;
        instructions.forEach(this::add);
    }

    private SymbolReference existingSymbol(CobolExpression expr) {
        Optional<InstructionQuad> any = quads.stream().filter(q -> q.result().equals(variableSymbolReferenceSearchSpec(expr))).findAny();
        if (any.isEmpty()) throw new UnresolvedSymbolReferenceException(expr);
        return any.get().result();
    }

    private ExpressionSymbolReference variableSymbolReferenceSearchSpec(CobolExpression expression) {
        return new ExpressionSymbolReference(expression, "-1");
    }

    public void add(InstructionQuad quad) {
        switch (quad.result()) {
            case ExpressionSymbolReference r:
                symbolTable.add(r);
                break;
            case AddressSymbolReference r:
                symbolTable.add(r);
                break;
            default:
                break;
        }
        quads.add(quad);
    }

    public SymbolReference lastResult() {
        return quads.getLast().result();
    }

    public void add(QuadSequence seq) {
        quads.addAll(seq.quads);
    }
}
