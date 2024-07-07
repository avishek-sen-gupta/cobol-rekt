package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public abstract class BinaryCobolOperatorExpression extends CobolExpression {
    protected CobolExpression rhs;
    protected CobolExpression lhs;

    public BinaryCobolOperatorExpression(CobolExpression lhs, CobolExpression rhs) {
        super(ImmutableList.of(lhs, rhs));
        this.rhs = rhs;
        this.lhs = lhs;
    }
}
