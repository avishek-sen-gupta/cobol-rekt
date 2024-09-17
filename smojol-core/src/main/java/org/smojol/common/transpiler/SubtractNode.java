package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

public class SubtractNode extends TranspilerNode {
    private final TranspilerNode minuend;
    private final TranspilerNode subtrahend;

    public SubtractNode(TranspilerNode minuend, TranspilerNode subtrahend) {
        super(ImmutableList.of(SemanticCategory.COMPUTATIONAL, SemanticCategory.DATA_FLOW));
        this.minuend = minuend;
        this.subtrahend = subtrahend;
    }

    @Override
    public String description() {
        return String.format("subtract(%s, %s)", minuend.description(), subtrahend.description());
    }
}
