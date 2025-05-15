package com.mojo.algorithms.transpiler;

import com.mojo.algorithms.domain.TranspilerNode;

public class LessThanNode extends TranspilerComparisonOperator {
    public LessThanNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("lt(%s, %s)", lhs.description(), rhs.description());
    }
}
