package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

public class NotNode extends TranspilerNode {
    private final TranspilerNode expression;

    public NotNode(TranspilerNode expression) {
        super(ImmutableList.of(SemanticCategory.RELATIONAL));
        this.expression = expression;
    }

    @Override
    public String description() {
        return String.format("not(%s)", expression.description());
    }
}
