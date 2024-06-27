package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public class DivideExpression extends CobolExpression {
    private final CobolExpression lhs;
    private final CobolExpression rhs;

    public DivideExpression(CobolExpression lhs, CobolExpression rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return lhs.evaluate(data).divide(rhs.evaluate(data), data);
    }
}
