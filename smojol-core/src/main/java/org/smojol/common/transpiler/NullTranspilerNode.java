package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

public class NullTranspilerNode extends TranspilerNode {
    public NullTranspilerNode() {
        super(ImmutableList.of(SemanticCategory.NULL));
    }

    @Override
    public String description() {
        return "NULL";
    }
}
