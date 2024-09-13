package org.smojol.common.transpiler;

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
    NotEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("neq(%s, %s)", lhs.description(), rhs.description());
    }
}

class GreaterThanOrEqualToNode extends TranspilerComparisonOperator {
    GreaterThanOrEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("geq(%s, %s)", lhs.description(), rhs.description());
    }
}

class LessThanOrEqualToNode extends TranspilerComparisonOperator {
    LessThanOrEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("leq(%s, %s)", lhs.description(), rhs.description());
    }
}

class GreaterThanNode extends TranspilerComparisonOperator {
    GreaterThanNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("gt(%s, %s)", lhs.description(), rhs.description());
    }
}

class LessThanNode extends TranspilerComparisonOperator {
    LessThanNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }

    @Override
    public String description() {
        return String.format("lt(%s, %s)", lhs.description(), rhs.description());
    }
}
