package org.smojol.common.transpiler;

public class SubtractNode extends TranspilerNode {
    private final TranspilerNode minuend;
    private final TranspilerNode subtrahend;

    public SubtractNode(TranspilerNode minuend, TranspilerNode subtrahend) {
        this.minuend = minuend;
        this.subtrahend = subtrahend;
    }

    @Override
    public String description() {
        return String.format("subtract(%s, %s)", minuend.description(), subtrahend.description());
    }
}
