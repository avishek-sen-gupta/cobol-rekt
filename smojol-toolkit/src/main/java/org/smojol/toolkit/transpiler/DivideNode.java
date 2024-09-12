package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class DivideNode extends TranspilerNode {
    private final TranspilerNode dividend;
    private final TranspilerNode divisor;

    public DivideNode(TranspilerNode dividend, TranspilerNode divisor) {
        this.dividend = dividend;
        this.divisor = divisor;
    }

    @Override
    public String description() {
        return String.format("divide(%s, %s)", dividend.description(), divisor.description());
    }
}
