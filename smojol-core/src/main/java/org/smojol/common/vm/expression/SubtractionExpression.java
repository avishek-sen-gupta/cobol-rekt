package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public class SubtractionExpression extends BinaryCobolOperatorExpression {
    public SubtractionExpression(CobolExpression lhs, CobolExpression rhs) {
        super(lhs, rhs, "SUBTRACT");
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return lhs.evaluate(data).subtract(rhs.evaluate(data), data);
    }
}
