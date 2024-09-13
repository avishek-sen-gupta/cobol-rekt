package org.smojol.common.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class EqualToNode extends TranspilerComparisonOperator {
    public EqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("eq(%s, %s)", lhs.description(), rhs.description());
    }
}

class NotEqualToNode extends TranspilerComparisonOperator {
    public NotEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("neq(%s, %s)", lhs.description(), rhs.description());
    }
}

class GreaterThanOrEqualToNode extends TranspilerComparisonOperator {
    public GreaterThanOrEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("geq(%s, %s)", lhs.description(), rhs.description());
    }
}

class LessThanOrEqualToNode extends TranspilerComparisonOperator {
    public LessThanOrEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("leq(%s, %s)", lhs.description(), rhs.description());
    }
}

class GreaterThanNode extends TranspilerComparisonOperator {
    public GreaterThanNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("gt(%s, %s)", lhs.description(), rhs.description());
    }
}

class LessThanNode extends TranspilerComparisonOperator {
    public LessThanNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("lt(%s, %s)", lhs.description(), rhs.description());
    }
}
