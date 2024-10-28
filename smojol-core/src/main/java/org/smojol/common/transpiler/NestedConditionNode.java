package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;

import java.util.Collection;

@Getter
public class NestedConditionNode extends TranspilerNode {
    private final TranspilerNode expression;

    public NestedConditionNode(TranspilerNode expression) {
        super(ImmutableList.of(SemanticCategory.RELATIONAL));
        this.expression = expression;
    }

    @Override
    public String description() {
        return String.format("nest(%s)", expression.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(expression);
    }
}
