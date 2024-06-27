package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public class AdditionExpression extends CobolExpression {
    private final CobolExpression lhs;
    private final CobolExpression rhs;

    public AdditionExpression(CobolExpression lhs, CobolExpression rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return lhs.evaluate(data).add(rhs.evaluate(data), data);
    }
}
