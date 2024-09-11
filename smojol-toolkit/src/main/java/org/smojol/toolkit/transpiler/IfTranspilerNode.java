package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

public class IfTranspilerNode implements TranspilerNode {
    private final TranspilerNode condition;
    private final TranspilerCodeBlock ifThenBlock;
    private final TranspilerCodeBlock ifElseBlock;

    public IfTranspilerNode(TranspilerNode condition, TranspilerCodeBlock ifThenBlock, TranspilerCodeBlock ifElseBlock) {
        this.condition = condition;
        this.ifThenBlock = ifThenBlock;
        this.ifElseBlock = ifElseBlock;
    }
}
