package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;

@Getter
public class ExponentExpression extends CobolExpression {
    private final CobolExpression basis;
    private final CobolExpression exponent;

    public ExponentExpression(CobolExpression basis, CobolExpression exponent) {
        super(ImmutableList.of(basis, exponent), "EXPONENT");
        this.basis = basis;
        this.exponent = exponent;
    }

    @Override
    public CobolExpression evaluate(CobolDataStructure data) {
        return basis.evaluate(data).exponent(exponent.evaluate(data), data);
    }

    @Override
    public String description() {
        return operationMnemonic + "(" + basis.description() + ", " + exponent.description() + ")";
    }

    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return AbstractCobolType.NUMBER;
    }
}
