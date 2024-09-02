package org.smojol.common.pseudocode;

public record InstructionQuad(SymbolReference result, AbstractOperator operator, SymbolReference lhs,
                              SymbolReference rhs) {
    public static InstructionQuad noOp() {
        return new InstructionQuad(new NullSymbolReference(), AbstractOperator.NO_OP, new NullSymbolReference(), new NullSymbolReference());
    }
}
