package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;

@Getter
public class MultiplyNode extends TranspilerNode {
    private final TranspilerNode lhs;
    private final TranspilerNode rhs;

    public MultiplyNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(ImmutableList.of(SemanticCategory.COMPUTATIONAL, SemanticCategory.DATA_FLOW));
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public String description() {
        return String.format("multiply(%s, %s)", lhs.description(), rhs.description());
    }
}
