package org.smojol.common.vm.expression;

import org.smojol.common.vm.structure.CobolDataStructure;

public class OrExpression extends BinaryCobolLogicExpression {
    public OrExpression(CobolExpression lhs, CobolExpression rhs) {
        super(lhs, rhs, "OR");
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return lhs.evaluate(data).or(rhs.evaluate(data), data);
    }
}
