package org.smojol.common.vm.expression;

public class ArithmeticExpression {
    public static CobolExpression expression(BinaryMathOperations operation, CobolExpression lhs, CobolExpression rhs) {
        return switch (operation) {
            case ADD -> new AdditionExpression(lhs, rhs);
            case SUBTRACT -> new SubtractionExpression(lhs, rhs);
            case MULTIPLY -> new MultiplyExpression(lhs, rhs);
            case DIVIDE -> new DivideExpression(lhs, rhs);
        };
//        throw new IllegalArgumentException("Unknown operation: " + operation);
    }

    public static CobolExpression expression(UnaryMathOperations operation, CobolExpression expression) {
        return switch (operation) {
            case POSITIVE -> expression;
            case NEGATIVE -> new NegativeExpression(expression);
        };
    }
}
