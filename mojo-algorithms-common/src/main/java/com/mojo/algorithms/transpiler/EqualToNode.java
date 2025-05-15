package com.mojo.algorithms.transpiler;


import com.mojo.algorithms.domain.TranspilerNode;

public class EqualToNode extends TranspilerComparisonOperator {
    public EqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("eq(%s, %s)", lhs.description(), rhs.description());
    }
}
