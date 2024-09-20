package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;

public abstract class ClassConditionExpression extends CobolExpression {
    @Getter protected final CobolExpression expression;

    public ClassConditionExpression(CobolExpression expression, String operationMnemonic) {
        super(ImmutableList.of(expression), operationMnemonic);
        this.expression = expression;
    }

    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return AbstractCobolType.BOOLEAN;
    }
}
