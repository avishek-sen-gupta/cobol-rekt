package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

public class OrTranspilerNode extends TranspilerNode {
    private final TranspilerNode lhs;
    private final TranspilerNode rhs;

    public OrTranspilerNode(TranspilerNode lhs, TranspilerNode rhs) {
        super(ImmutableList.of(SemanticCategory.RELATIONAL));
        this.lhs = lhs;
        this.rhs = rhs;
    }

    @Override
    public String description() {
        return String.format("or(%s, %s)", lhs.description(), rhs.description());
    }
}
