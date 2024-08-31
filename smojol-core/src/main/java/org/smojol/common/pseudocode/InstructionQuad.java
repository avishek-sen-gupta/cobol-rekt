package org.smojol.common.pseudocode;

public record InstructionQuad(SymbolReference result, AbstractOperator operator, SymbolReference lhs, SymbolReference rhs) {
}
