package org.smojol.common.transpiler;

import java.util.List;

public class LabelledTranspilerCodeBlockNode extends TranspilerNode {
    private final String name;
    private final List<TranspilerNode> children;

    public LabelledTranspilerCodeBlockNode(String name, List<TranspilerNode> children) {
        this.name = name;
        this.children = children;
    }

    @Override
    public String description() {
        return String.format("[%s]", name);
    }
}
