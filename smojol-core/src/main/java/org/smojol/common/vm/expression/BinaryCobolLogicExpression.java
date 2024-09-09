package org.smojol.common.vm.expression;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import lombok.Setter;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.AbstractCobolType;

@Setter
@Getter
public abstract class BinaryCobolLogicExpression extends CobolExpression {
    protected CobolExpression rhs;
    protected CobolExpression lhs;

    public BinaryCobolLogicExpression(CobolExpression lhs, CobolExpression rhs, String operationMnemonic) {
        super(ImmutableList.of(lhs, rhs), operationMnemonic);
        this.rhs = rhs;
        this.lhs = lhs;
    }

    @Override
    public String description() {
        return operationMnemonic + "(" + lhs.description() + ", " + rhs.description() + ")";
    }

    @Override
    public AbstractCobolType expressionType(CobolDataStructure dataStructures) {
        return AbstractCobolType.BOOLEAN;
    }
}
