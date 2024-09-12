package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class TranspilerLoopUpdate extends TranspilerNode {
    private final TranspilerNode updateExpression;

    public TranspilerLoopUpdate(TranspilerNode updateExpression) {
        this.updateExpression = updateExpression;
    }

    @Override
    public String description() {
        return updateExpression.description();
    }
}
