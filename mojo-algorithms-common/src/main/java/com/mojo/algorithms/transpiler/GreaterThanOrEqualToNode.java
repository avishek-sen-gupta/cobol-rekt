package com.mojo.algorithms.transpiler;

import com.mojo.algorithms.domain.TranspilerNode;

public class GreaterThanOrEqualToNode extends TranspilerComparisonOperator {
    public GreaterThanOrEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("geq(%s, %s)", lhs.description(), rhs.description());
    }
}
