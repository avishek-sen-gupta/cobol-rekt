package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;

import java.util.Collection;

@Getter
public class ValueOfNode extends TranspilerNode {
    private final TranspilerNode expression;

    public ValueOfNode(TranspilerNode expression) {
        super(ImmutableList.of(SemanticCategory.DEREFERENCE));
        this.expression = expression;
    }

    @Override
    public String description() {
        return String.format("value(%s)", expression.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(expression);
    }
}
