package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

import java.util.Collection;
import java.util.List;

@Getter
public class PrintTranspilerNode extends TranspilerNode {
    private final List<TranspilerNode> operands;

    public PrintTranspilerNode(List<TranspilerNode> operands) {
        super(ImmutableList.of(SemanticCategory.IO));
        this.operands = operands;
    }

    @Override
    public String description() {
        return String.format("print(%s)", String.join(", ", operands.stream().map(TranspilerNode::description).toList()));
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return operands;
    }
}
