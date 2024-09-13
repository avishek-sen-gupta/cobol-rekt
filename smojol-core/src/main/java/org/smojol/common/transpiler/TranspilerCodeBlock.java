package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;

import java.util.ArrayList;
import java.util.List;

public class TranspilerCodeBlock extends TranspilerNode {
    private final List<TranspilerNode> children = new ArrayList<>();

    public TranspilerCodeBlock(List<TranspilerNode> children) {
        this.children.addAll(children);
    }

    public TranspilerCodeBlock() {
        this(ImmutableList.of());
    }

    public TranspilerCodeBlock(TranspilerNode single) {
        this.children.add(single);
    }

    public boolean isEmpty() {
        return children.isEmpty();
    }

    public TranspilerNode unwrap() {
        return children.size() > 1 ? this : children.getFirst();
    }

    @Override
    public String description() {
        return String.join("\n", children.stream().map(TranspilerNode::description).toList());
    }
}
