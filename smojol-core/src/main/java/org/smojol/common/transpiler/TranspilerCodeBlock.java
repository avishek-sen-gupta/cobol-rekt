package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class TranspilerCodeBlock extends TranspilerNode {
    private final List<TranspilerNode> children = new ArrayList<>();

    public TranspilerCodeBlock(List<TranspilerNode> children) {
        super(ImmutableList.of(SemanticCategory.CODE_BLOCK));
        this.children.addAll(children);
    }

    public TranspilerCodeBlock() {
        this(ImmutableList.of());
    }

    public TranspilerCodeBlock(TranspilerNode single) {
        super(ImmutableList.of(SemanticCategory.CODE_BLOCK));
        this.children.add(single);
    }

    public boolean isEmpty() {
        return children.isEmpty();
    }

    public TranspilerNode unwrap() {
        return children.size() != 1 ? this : children.getFirst();
    }

    @Override
    public String description() {
        return String.join("\n", children.stream().map(TranspilerNode::description).toList());
    }

    @Override
    public Collection<TranspilerNode> astChildren() {
        return children;
    }

    public void add(TranspilerNode node) {
        children.add(node);
    }
}
