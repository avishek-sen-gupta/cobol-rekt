package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import org.smojol.common.vm.structure.CobolDataStructure;

public class PositiveExpression extends CobolExpression {
    private final CobolExpression expression;

    public PositiveExpression(CobolExpression expression) {
        super(ImmutableList.of(expression));
        this.expression = expression;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        // TODO: Keep an eye on this. Harmless, but still...
        return expression;
    }
}
