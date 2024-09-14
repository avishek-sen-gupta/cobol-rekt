package org.smojol.common.transpiler;

import org.smojol.common.pseudocode.CodeSentinelType;

import java.util.List;

public class LabelledTranspilerCodeBlockNode extends TranspilerNode {
    private final String name;
    private final CodeSentinelType codeSentinelType;
    private final List<TranspilerNode> children;

    public LabelledTranspilerCodeBlockNode(String name, List<TranspilerNode> children, CodeSentinelType codeSentinelType) {
        this.name = name;
        this.codeSentinelType = codeSentinelType;
        this.children = children;
    }

    @Override
    public String description() {
        return String.format("[%s] %s", codeSentinelType, name);
    }
}
