package org.smojol.common.vm.expression;

public class LoopUpdate {
    private final CobolExpression updateDelta;

    public LoopUpdate(CobolExpression updateDelta) {
        this.updateDelta = updateDelta;
    }
}
