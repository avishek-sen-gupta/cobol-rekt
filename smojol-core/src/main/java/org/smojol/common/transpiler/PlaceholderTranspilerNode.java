package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

public class PlaceholderTranspilerNode extends TranspilerNode {
    private final String s;

    public PlaceholderTranspilerNode(String s) {
        super(ImmutableList.of(SemanticCategory.UNKNOWN));
        this.s = s;
    }

    @Override
    public String description() {
        return "placeholder: " + s;
    }
}
