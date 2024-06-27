package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public class MultiplyExpression extends CobolExpression {
    private final CobolExpression lhs;
    private final CobolExpression rhs;

    public MultiplyExpression(CobolExpression lhs, CobolExpression rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return lhs.evaluate(data).multiply(rhs.evaluate(data), data);
    }
}
