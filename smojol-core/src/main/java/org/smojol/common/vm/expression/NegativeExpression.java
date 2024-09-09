package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;

public class NegativeExpression extends CobolExpression {
    @Getter private final CobolExpression expression;

    public NegativeExpression(CobolExpression expression) {
        super(ImmutableList.of(expression), "NEGATIVE");
        this.expression = expression;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return expression.evaluate(data).negative(data);
    }

    @Override
    public String description() {
        return operationMnemonic + "(" + expression.description() + ")";
    }

    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return AbstractCobolType.NUMBER;
    }
}
