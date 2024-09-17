package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

import java.util.Collection;

public class IfTranspilerNode extends TranspilerNode {
    private final TranspilerNode condition;
    private final TranspilerNode ifThenBlock;
    private final TranspilerNode ifElseBlock;

    public IfTranspilerNode(TranspilerNode condition, TranspilerCodeBlock ifThenBlock, TranspilerCodeBlock ifElseBlock) {
        super(ImmutableList.of(SemanticCategory.DECISION));
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

    @Override
    public Collection<TranspilerNode> astChildren() {
        return ImmutableList.of(ifThenBlock, ifElseBlock);
    }
}
