package org.smojol.common.vm.expression;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public abstract class BinaryCobolLogicExpression extends CobolExpression {
    protected CobolExpression rhs;
    protected CobolExpression lhs;

    public BinaryCobolLogicExpression(CobolExpression lhs, CobolExpression rhs) {
        this.rhs = rhs;
        this.lhs = lhs;
    }
}
