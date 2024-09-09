package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;

public class NotExpression extends CobolExpression {
    @Getter private final CobolExpression expression;

    public NotExpression(CobolExpression expression) {
        super(ImmutableList.of(expression), "NOT");
        this.expression = expression;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return expression.evaluate(data).not(data);
    }

    @Override
    public String description() {
        return operationMnemonic + "(" + expression.description() + ")";
    }

    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return AbstractCobolType.BOOLEAN;
    }
}
