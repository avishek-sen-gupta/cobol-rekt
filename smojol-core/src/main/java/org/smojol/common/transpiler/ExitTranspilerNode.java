package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

public class ExitTranspilerNode extends TranspilerNode {
    public ExitTranspilerNode() {
        super(ImmutableList.of(SemanticCategory.TERMINAL));
    }

    @Override
    public String description() {
        return "[EXIT]";
    }
}
