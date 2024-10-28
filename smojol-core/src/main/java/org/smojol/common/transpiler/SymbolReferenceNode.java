package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;

@Getter
public class SymbolReferenceNode extends TranspilerNode {
    private final String name;

    public SymbolReferenceNode(String name) {
        super(ImmutableList.of(SemanticCategory.REFERENCE));
        this.name = name;
    }

    @Override
    public String description() {
        return String.format("ref('%s')", name);
    }
}
