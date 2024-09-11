package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class ExponentNode implements TranspilerNode {
    private final TranspilerNode basis;
    private final TranspilerNode exponent;

    public ExponentNode(TranspilerNode basis, TranspilerNode exponent) {
        this.basis = basis;
        this.exponent = exponent;
    }
}
