package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class DivideNode implements TranspilerNode {
    private final TranspilerNode dividend;
    private final TranspilerNode divisor;

    public DivideNode(TranspilerNode dividend, TranspilerNode divisor) {
        this.dividend = dividend;
        this.divisor = divisor;
    }
}
