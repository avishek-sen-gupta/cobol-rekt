package org.smojol.common.vm.expression;

public interface CobolExpressionVisitor {
    CobolExpressionVisitor visit(CobolExpression expression);
}
