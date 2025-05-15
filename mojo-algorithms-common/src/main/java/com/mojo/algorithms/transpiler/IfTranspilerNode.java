package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

import java.util.Collection;
import java.util.List;

@Getter
public class IfTranspilerNode extends TranspilerNode {
    private final TranspilerNode condition;
    private final TranspilerNode ifThenBlock;
    private final TranspilerNode ifElseBlock;

    public IfTranspilerNode(TranspilerNode condition, TranspilerNode ifThenBlock, TranspilerNode ifElseBlock) {
        super(ImmutableList.of(ifThenBlock, ifElseBlock), ImmutableList.of(SemanticCategory.DECISION));
        this.condition = condition;
        this.ifThenBlock = ifThenBlock;
        this.ifElseBlock = ifElseBlock;
    }

    public IfTranspilerNode(TranspilerNode condition, List<TranspilerNode> allBlocks) {
        super(allBlocks, ImmutableList.of(SemanticCategory.DECISION));
        this.condition = condition;
        this.ifThenBlock = allBlocks.getFirst();
        this.ifElseBlock = allBlocks.get(1);
    }

    public IfTranspilerNode(TranspilerNode condition, TranspilerNode ifThenBlock) {
        this(condition, ifThenBlock, new DetachedTranspilerCodeBlockNode());
    }

    @Override
    public String description() {
        return String.format("if(%s) %n then %n{%n %s %n}%n %nelse %n{%n %s %n}%n", condition.description(), ifThenBlock.description(), ifElseBlock.description());
    }

    @Override
    public List<TranspilerNode> astChildren() {
        return super.astChildren();
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of(condition, ifThenBlock, ifElseBlock);
    }
}
