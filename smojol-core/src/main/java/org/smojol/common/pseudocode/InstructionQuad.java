package org.smojol.common.pseudocode;

public record InstructionQuad(SymbolReference result, AbstractOperator operator, SymbolReference lhs,
                              SymbolReference rhs) {
    public InstructionQuad(SymbolReference result, AbstractOperator abstractOperator, SymbolReference lhs) {
        this(result, abstractOperator, lhs, new NullSymbolReference());
    }

    public InstructionQuad(SymbolReference result, AbstractOperator abstractOperator) {
        this(result, abstractOperator, new NullSymbolReference());
    }

    public static InstructionQuad noOp() {
        return new InstructionQuad(new NullSymbolReference(), AbstractOperator.NO_OP, new NullSymbolReference(), new NullSymbolReference());
    }
}
