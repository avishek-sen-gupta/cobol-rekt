package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

import java.util.Collection;
import java.util.List;

public class LabelledTranspilerCodeBlockNode extends TranspilerNode {
    private final String name;
    private final List<TranspilerNode> children;

    public LabelledTranspilerCodeBlockNode(String name, List<TranspilerNode> children) {
        super(ImmutableList.of(SemanticCategory.CODE_BLOCK));
        this.name = name;
        this.children = children;
    }

    @Override
    public String description() {
        return String.format("[%s]", name);
    }

    @Override
    public Collection<TranspilerNode> astChildren() {
        return children;
    }
}
