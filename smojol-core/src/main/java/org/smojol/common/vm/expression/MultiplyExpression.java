package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public class MultiplyExpression extends BinaryCobolOperatorExpression {
    public MultiplyExpression(CobolExpression lhs, CobolExpression rhs) {
        super(lhs, rhs, "MULTIPLY");
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return lhs.evaluate(data).multiply(rhs.evaluate(data), data);
    }
}
