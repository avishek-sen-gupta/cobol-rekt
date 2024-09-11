package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

class EqualToNode extends TranspilerComparisonOperator {
    public EqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }
}

class NotEqualToNode extends TranspilerComparisonOperator {
    public NotEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }
}

class GreaterThanOrEqualToNode extends TranspilerComparisonOperator {
    public GreaterThanOrEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }
}

class LessThanOrEqualToNode extends TranspilerComparisonOperator {
    public LessThanOrEqualToNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }
}

class GreaterThanNode extends TranspilerComparisonOperator {
    public GreaterThanNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }
}

class LessThanNode extends TranspilerComparisonOperator {
    public LessThanNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(lhs, rhs);
    }
}
