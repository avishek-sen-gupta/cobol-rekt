package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;

import java.util.Collection;

@Getter
public class AndTranspilerNode extends TranspilerNode {
    private final TranspilerNode lhs;
    private final TranspilerNode rhs;

    public AndTranspilerNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(ImmutableList.of(SemanticCategory.RELATIONAL));
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public String description() {
        return String.format("and(%s, %s)", lhs.description(), rhs.description());
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(lhs, rhs);
    }
}
