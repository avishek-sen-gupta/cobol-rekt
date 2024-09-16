package org.smojol.common.transpiler;

public class IfTranspilerNode extends TranspilerNode {
    private final TranspilerNode condition;
    private final TranspilerNode ifThenBlock;
    private final TranspilerNode ifElseBlock;

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
        return String.format("if(%s) %n then %n{%n %s %n}%n %nelse %n{%n %s %n}%n", condition.description(), ifThenBlock.description(), ifElseBlock.description());
    }
}
