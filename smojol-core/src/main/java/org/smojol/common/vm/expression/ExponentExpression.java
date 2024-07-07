package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.smojol.common.vm.structure.CobolDataStructure;

public class ExponentExpression extends CobolExpression {
    private final CobolExpression basis;
    private final CobolExpression exponent;

    public ExponentExpression(CobolExpression basis, CobolExpression exponent) {
        super(ImmutableList.of(basis, exponent));
        this.basis = basis;
        this.exponent = exponent;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return basis.evaluate(data).exponent(exponent.evaluate(data), data);
    }
}
