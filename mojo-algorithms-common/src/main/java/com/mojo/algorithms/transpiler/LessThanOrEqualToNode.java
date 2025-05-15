package com.mojo.algorithms.transpiler;

import com.mojo.algorithms.domain.TranspilerNode;

public class LessThanOrEqualToNode extends TranspilerComparisonOperator {
    public LessThanOrEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("leq(%s, %s)", lhs.description(), rhs.description());
    }
}

