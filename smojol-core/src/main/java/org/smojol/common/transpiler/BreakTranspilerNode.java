package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

public class BreakTranspilerNode extends TranspilerNode {
    public BreakTranspilerNode() {
        super(ImmutableList.of(SemanticCategory.CONTROL_FLOW));
    }

    @Override
    public String description() {
        return "break()";
    }
}
