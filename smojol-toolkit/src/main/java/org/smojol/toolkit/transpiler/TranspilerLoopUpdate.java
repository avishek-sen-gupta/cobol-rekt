package org.smojol.toolkit.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;
import org.smojol.common.transpiler.TranspilerNode;

public class TranspilerLoopUpdate extends TranspilerNode {
    private final TranspilerNode updateExpression;

    public TranspilerLoopUpdate(TranspilerNode updateExpression) {
        super(ImmutableList.of(SemanticCategory.ITERATION, SemanticCategory.DATA_FLOW));
        this.updateExpression = updateExpression;
    }

    @Override
    public String description() {
        return updateExpression.description();
    }
}
