package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public abstract class BinaryCobolOperatorExpression extends CobolExpression {
    protected CobolExpression rhs;
    protected CobolExpression lhs;

    public BinaryCobolOperatorExpression(CobolExpression lhs, CobolExpression rhs, String operationMnemonic) {
        super(ImmutableList.of(lhs, rhs), operationMnemonic);
        this.rhs = rhs;
        this.lhs = lhs;
    }

    @Override
    public String description() {
        return operationMnemonic + "(" + lhs.description() + ", " + rhs.description() + ")";
    }
}
