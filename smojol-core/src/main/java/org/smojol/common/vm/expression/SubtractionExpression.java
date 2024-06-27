package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public class SubtractionExpression extends CobolExpression {
    private final CobolExpression lhs;
    private final CobolExpression rhs;

    public SubtractionExpression(CobolExpression lhs, CobolExpression rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return lhs.evaluate(data).subtract(rhs.evaluate(data), data);
    }
}
