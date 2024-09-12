package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class IfTranspilerNode extends TranspilerNode {
    private final TranspilerNode condition;
    private final TranspilerCodeBlock ifThenBlock;
    private final TranspilerCodeBlock ifElseBlock;

    public IfTranspilerNode(TranspilerNode condition, TranspilerCodeBlock ifThenBlock, TranspilerCodeBlock ifElseBlock) {
        this.condition = condition;
        this.ifThenBlock = ifThenBlock;
        this.ifElseBlock = ifElseBlock;
    }

    public IfTranspilerNode(TranspilerNode condition, TranspilerCodeBlock ifThenBlock) {
        this(condition, ifThenBlock, new TranspilerCodeBlock());
    }

    @Override
    public String description() {
        return String.format("if(%s) then (%s) else (%s)", condition.description(), ifThenBlock.description(), ifElseBlock.description());
    }
}
