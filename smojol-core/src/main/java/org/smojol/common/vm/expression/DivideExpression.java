package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public class DivideExpression extends BinaryCobolOperatorExpression {
    public DivideExpression(CobolExpression lhs, CobolExpression rhs) {
        super(lhs, rhs, "DIVIDE");
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return lhs.evaluate(data).divide(rhs.evaluate(data), data);
    }
}
