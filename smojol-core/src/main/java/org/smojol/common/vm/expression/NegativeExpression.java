package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.smojol.common.vm.structure.CobolDataStructure;

public class NegativeExpression extends CobolExpression {
    private final CobolExpression expression;

    public NegativeExpression(CobolExpression expression) {
        super(ImmutableList.of(expression));
        this.expression = expression;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return expression.evaluate(data).negative(data);
    }
}
