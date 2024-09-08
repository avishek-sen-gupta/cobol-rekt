package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;

public abstract class ClassConditionExpression extends CobolExpression {
    protected final CobolExpression expression;

    public ClassConditionExpression(CobolExpression expression, String operationMnemonic) {
        super(ImmutableList.of(expression), operationMnemonic);
        this.expression = expression;
    }
}
