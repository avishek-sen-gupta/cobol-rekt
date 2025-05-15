package com.mojo.algorithms.transpiler;

import com.mojo.algorithms.domain.TranspilerNode;

public class NotEqualToNode extends TranspilerComparisonOperator {
    public NotEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("neq(%s, %s)", lhs.description(), rhs.description());
    }
}
