package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.expression.ConditionTestTime;

import java.util.Objects;

public final class TranspilerLoop extends TranspilerNode {
    private final TranspilerNode loopVariable;
    private final TranspilerNode initialValue;
    private final TranspilerNode maxValue;
    private final TranspilerNode terminateCondition;
    private final TranspilerNode loopUpdate;
    private final ConditionTestTime conditionTestTime;
    private final TranspilerNode body;

    public TranspilerLoop(TranspilerNode loopVariable, TranspilerNode initialValue, TranspilerNode maxValue,
                          TranspilerNode terminateCondition, TranspilerNode loopUpdate,
                          ConditionTestTime conditionTestTime, TranspilerNode body) {
        this.loopVariable = loopVariable;
        this.initialValue = initialValue;
        this.maxValue = maxValue;
        this.terminateCondition = terminateCondition;
        this.loopUpdate = loopUpdate;
        this.conditionTestTime = conditionTestTime;
        this.body = body;
    }

    public TranspilerNode loopVariable() {
        return loopVariable;
    }

    public TranspilerNode initialValue() {
        return initialValue;
    }

    public TranspilerNode maxValue() {
        return maxValue;
    }

    public TranspilerNode terminateCondition() {
        return terminateCondition;
    }

    public TranspilerNode loopUpdate() {
        return loopUpdate;
    }

    public ConditionTestTime conditionTestTime() {
        return conditionTestTime;
    }

    public TranspilerNode body() {
        return body;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        var that = (TranspilerLoop) obj;
        return Objects.equals(this.loopVariable, that.loopVariable) &&
                Objects.equals(this.initialValue, that.initialValue) &&
                Objects.equals(this.maxValue, that.maxValue) &&
                Objects.equals(this.terminateCondition, that.terminateCondition) &&
                Objects.equals(this.loopUpdate, that.loopUpdate) &&
                Objects.equals(this.conditionTestTime, that.conditionTestTime) &&
                Objects.equals(this.body, that.body);
    }

    @Override
    public int hashCode() {
        return Objects.hash(loopVariable, initialValue, maxValue, terminateCondition, loopUpdate, conditionTestTime, body);
    }

    @Override
    public String toString() {
        return "TranspilerLoop[" +
                "loopVariable=" + loopVariable + ", " +
                "initialValue=" + initialValue + ", " +
                "maxValue=" + maxValue + ", " +
                "terminateCondition=" + terminateCondition + ", " +
                "loopUpdate=" + loopUpdate + ", " +
                "conditionTestTime=" + conditionTestTime + ", " +
                "body=" + body + ']';
    }

    @Override
    public String description() {
        return "";
    }

}
