package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;

@Getter
public class JumpIfTranspilerNode extends TranspilerNode {
    private final LocationNode destination;
    private final TranspilerNode condition;

    public JumpIfTranspilerNode(LocationNode location, TranspilerNode condition) {
        super(ImmutableList.of(SemanticCategory.CONTROL_FLOW));
        this.destination = location;
        this.condition = condition;
    }

    @Override
    public String description() {
        return String.format("jump_if(%s, %s)", condition.description(), destination.description());
    }
}
