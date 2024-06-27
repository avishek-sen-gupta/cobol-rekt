package org.smojol.common.vm.expression;

public abstract class ClassConditionExpression extends CobolExpression {
    protected final CobolExpression expression;

    public ClassConditionExpression(CobolExpression expression) {
        this.expression = expression;
    }
}
