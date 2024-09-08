package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public class AndExpression extends BinaryCobolLogicExpression {
    public AndExpression(CobolExpression lhs, CobolExpression rhs) {
        super(lhs, rhs, "AND");
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return lhs.evaluate(data).and(rhs.evaluate(data), data);
    }
}
