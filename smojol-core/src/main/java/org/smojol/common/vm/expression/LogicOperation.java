package org.smojol.common.vm.expression;

public enum LogicOperation {
    AND, OR;

    public static CobolExpression create(LogicOperation logicOperation, CobolExpression lhs, CobolExpression rhs) {
        return switch (logicOperation) {
            case AND -> new AndExpression(lhs, rhs);
            case OR -> new OrExpression(lhs, rhs);
        };
    }
}
