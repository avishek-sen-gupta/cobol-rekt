package org.smojol.common.vm.expression;

import org.smojol.common.vm.type.TypedRecord;

import static org.smojol.common.vm.expression.ConditionTestTime.BEFORE;

public record FlowIteration(CobolExpression loopVariable,
                            CobolExpression initialValue,
                            CobolExpression maxValue,
                            CobolExpression condition,
                            LoopUpdate loopUpdate,
                            ConditionTestTime conditionTestTime
) {
    public static FlowIteration withMaxValue(CobolExpression loopVariable, CobolExpression initialValue, CobolExpression maxValue, LoopUpdate loopUpdate, ConditionTestTime conditionTestTime) {
        return new FlowIteration(loopVariable, initialValue, maxValue, new NullCobolExpression("NULL"), loopUpdate, conditionTestTime);
    }

    public static FlowIteration whileLoop(CobolExpression condition, ConditionTestTime conditionTestTime) {
        return new FlowIteration(new NullCobolExpression("NULL"), new NullCobolExpression("NULL"),
                new NullCobolExpression("NULL"), condition, new LoopUpdate(new NullCobolExpression("NULL")),
                conditionTestTime);
    }

    public static FlowIteration times(CobolExpression times) {
        return new FlowIteration(new VariableExpression("TEMP"), new PrimitiveCobolExpression(TypedRecord.typedNumber(1)),
                times, new NullCobolExpression("NULL"),
                new LoopUpdate(new PrimitiveCobolExpression(TypedRecord.typedNumber(1))),
                BEFORE);
    }
}
