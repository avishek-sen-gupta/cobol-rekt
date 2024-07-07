package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.smojol.common.vm.structure.CobolDataStructure;

public class NotExpression extends CobolExpression {
    private final CobolExpression expression;

    public NotExpression(CobolExpression expression) {
        super(ImmutableList.of(expression));
        this.expression = expression;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return expression.evaluate(data).not(data);
    }
}
