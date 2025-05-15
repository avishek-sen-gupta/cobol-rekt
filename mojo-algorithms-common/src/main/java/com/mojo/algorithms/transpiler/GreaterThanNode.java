package com.mojo.algorithms.transpiler;

import com.mojo.algorithms.domain.TranspilerNode;

public class GreaterThanNode extends TranspilerComparisonOperator {
    public GreaterThanNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("gt(%s, %s)", lhs.description(), rhs.description());
    }
}
