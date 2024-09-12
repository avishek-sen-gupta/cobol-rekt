package org.smojol.toolkit.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.transpiler.TranspilerNode;

import java.util.List;

public class TranspilerCodeBlock extends TranspilerNode {
    private final List<TranspilerNode> children;

    public TranspilerCodeBlock(List<TranspilerNode> children) {
        this.children = children;
    }

    public TranspilerCodeBlock() {
        this(ImmutableList.of());
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
