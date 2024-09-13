package org.smojol.common.transpiler;

import org.smojol.common.pseudocode.CodeSentinelType;

public class LabelledTranspilerCodeBlockNode extends TranspilerNode {
    private final String name;
    private final CodeSentinelType codeSentinelType;

    public LabelledTranspilerCodeBlockNode(String name, CodeSentinelType codeSentinelType) {
        this.name = name;
        this.codeSentinelType = codeSentinelType;
    }

    @Override
    public String description() {
        return String.format("[%s] %s", codeSentinelType, name);
    }
}
