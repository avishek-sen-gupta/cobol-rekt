package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;

import java.util.List;

import static org.smojol.common.format.TextFormat.truncated;

public class DetachedTranspilerCodeBlock extends TranspilerCodeBlock {
    public DetachedTranspilerCodeBlock(List<TranspilerNode> children) {
        super(children);
    }

    public DetachedTranspilerCodeBlock() {
        this(ImmutableList.of());
    }

    public DetachedTranspilerCodeBlock(TranspilerNode single) {
        super(single);
    }

    public boolean isEmpty() {
        return childTranspilerNodes.isEmpty();
    }

    public TranspilerNode unwrap() {
        return childTranspilerNodes.size() != 1 ? this : childTranspilerNodes.getFirst();
    }

    @Override
    public String description() {
//        return "CODE_BLOCK: " + id;
        return "CODE_BLOCK: " + String.join("\n", childTranspilerNodes.stream().map(TranspilerNode::description).toList());
    }

    public void add(TranspilerNode node) {
        childTranspilerNodes.add(node);
    }

    @Override
    public String shortDescription() {
        return truncated(description(), 30);
    }
}
