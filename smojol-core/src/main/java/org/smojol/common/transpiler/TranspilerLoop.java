package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;
import org.smojol.common.vm.expression.ConditionTestTime;

import java.util.Collection;
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
        super(ImmutableList.of(SemanticCategory.ITERATION));
        this.loopVariable = loopVariable;
        this.initialValue = initialValue;
        this.maxValue = maxValue;
        this.terminateCondition = terminateCondition;
        this.loopUpdate = loopUpdate;
        this.conditionTestTime = conditionTestTime;
        this.body = body;
    }

    public TranspilerNode body() {
        return body;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        var that = (TranspilerLoop) obj;
        return Objects.equals(this.loopVariable, that.loopVariable)
                && Objects.equals(this.initialValue, that.initialValue)
                && Objects.equals(this.maxValue, that.maxValue)
                && Objects.equals(this.terminateCondition, that.terminateCondition)
                && Objects.equals(this.loopUpdate, that.loopUpdate)
                && Objects.equals(this.conditionTestTime, that.conditionTestTime)
                && Objects.equals(this.body, that.body);
    }

    @Override
    public int hashCode() {
        return Objects.hash(loopVariable, initialValue, maxValue, terminateCondition, loopUpdate, conditionTestTime, body);
    }

    @Override
    public String description() {
        return "loop["
                + "loopVariable=" + loopVariable.description() + ", "
                + "initialValue=" + initialValue.description() + ", "
                + "maxValue=" + maxValue.description() + ", "
                + "terminateCondition=" + terminateCondition.description() + ", "
                + "loopUpdate=" + loopUpdate.description() + ", "
                + "conditionTestTime=" + conditionTestTime.name() + "]";
    }

    @Override
    public Collection<TranspilerNode> astChildren() {
        return ImmutableList.of(body);
    }
}
