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
    public static FlowIteration withCondition(CobolExpression loopVariable, CobolExpression initialValue, CobolExpression condition, LoopUpdate loopUpdate, ConditionTestTime conditionTestTime) {
//        return new FlowIteration(loopVariable, initialValue, maxValue, new NullCobolExpression("NULL"), loopUpdate, conditionTestTime);
//        return new FlowIteration(loopVariable, initialValue, condition, new SimpleConditionExpression(loopVariable, new RelationExpression(RelationalOperation.EQUAL, condition)), loopUpdate, conditionTestTime);
        return new FlowIteration(loopVariable, initialValue, new NullCobolExpression("NULL"), condition, loopUpdate, conditionTestTime);
    }

    public static FlowIteration whileLoop(CobolExpression condition, ConditionTestTime conditionTestTime) {
        return new FlowIteration(new NullCobolExpression("NULL"), new NullCobolExpression("NULL"),
                new NullCobolExpression("NULL"), condition, new LoopUpdate(new NullCobolExpression("NULL")),
                conditionTestTime);
    }

    public static FlowIteration times(CobolExpression times) {
        VariableExpression temp = new VariableExpression("TEMP");
        return new FlowIteration(temp, new PrimitiveCobolExpression(TypedRecord.typedNumber(1)),
                times, new SimpleConditionExpression(temp, new RelationExpression(RelationalOperation.EQUAL, times)),
                new LoopUpdate(new PrimitiveCobolExpression(TypedRecord.typedNumber(1))),
                BEFORE);
    }
}
