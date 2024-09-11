package org.smojol.common.transpiler;

public class ValueOfNode implements TranspilerNode {
    private final TranspilerNode expression;

    public ValueOfNode(TranspilerNode expression) {
        this.expression = expression;
    }
}
