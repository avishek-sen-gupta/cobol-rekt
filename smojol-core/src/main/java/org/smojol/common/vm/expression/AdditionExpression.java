package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public class AdditionExpression extends BinaryCobolOperatorExpression {
    public AdditionExpression(CobolExpression lhs, CobolExpression rhs) {
        super(lhs, rhs, "ADD");
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return lhs.evaluate(data).add(rhs.evaluate(data), data);
    }
}
