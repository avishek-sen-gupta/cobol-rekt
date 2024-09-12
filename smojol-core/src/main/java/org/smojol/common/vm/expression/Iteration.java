package org.smojol.common.vm.expression;

import org.smojol.common.vm.type.TypedRecord;

import static org.smojol.common.vm.expression.ConditionTestTime.BEFORE;

public class Iteration extends FlowLoop {
    private final CobolExpression loopVariable;
    private final CobolExpression initialValue;
    private final CobolExpression maxValue;
    private final LoopUpdate loopUpdate;
    private final ConditionTestTime conditionTestTime;
    private CobolExpression condition;

    public static Iteration withMaxValue(CobolExpression loopVariable, CobolExpression initialValue, CobolExpression maxValue, LoopUpdate loopUpdate, ConditionTestTime conditionTestTime) {
        return new Iteration(loopVariable, initialValue, maxValue, new NullCobolExpression("NULL"), loopUpdate, conditionTestTime);
    }

    public static Iteration withCondition(CobolExpression loopVariable, CobolExpression initialValue, CobolExpression condition, LoopUpdate loopUpdate, ConditionTestTime conditionTestTime) {
        return new Iteration(loopVariable, initialValue, new NullCobolExpression("NULL"), condition, loopUpdate, conditionTestTime);
    }

    public static Iteration whileLoop(CobolExpression condition, ConditionTestTime conditionTestTime) {
        return new Iteration(new NullCobolExpression("NULL"), new NullCobolExpression("NULL"),
                new NullCobolExpression("NULL"), condition, new LoopUpdate(new NullCobolExpression("NULL")),
                conditionTestTime);
    }

    public Iteration(CobolExpression loopVariable, CobolExpression initialValue, CobolExpression maxValue, CobolExpression condition, LoopUpdate loopUpdate, ConditionTestTime conditionTestTime) {
        this.loopVariable = loopVariable;
        this.initialValue = initialValue;
        this.maxValue = maxValue;
        this.condition = condition;
        this.loopUpdate = loopUpdate;
        this.conditionTestTime = conditionTestTime;
    }

    public Iteration(CobolExpression times) {
        this(new VariableExpression("TEMP"), new PrimitiveCobolExpression(TypedRecord.typedNumber(1)),
                times, new NullCobolExpression("NULL"),
                new LoopUpdate(new PrimitiveCobolExpression(TypedRecord.typedNumber(1))),
                BEFORE);
    }
}
