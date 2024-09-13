package org.smojol.common.transpiler;

public class ExponentNode extends TranspilerNode {
    private final TranspilerNode basis;
    private final TranspilerNode exponent;

    public ExponentNode(TranspilerNode basis, TranspilerNode exponent) {
        this.basis = basis;
        this.exponent = exponent;
    }

    @Override
    public String description() {
        return String.format("exp(%s, %s)", basis.description(), exponent.description());
    }
}
