package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;

import java.util.List;

public class PrintTranspilerNode extends TranspilerNode {
    private final List<TranspilerNode> operands;

    public PrintTranspilerNode(List<TranspilerNode> operands) {
        this.operands = operands;
    }

    @Override
    public String description() {
        return String.format("print(%s)", String.join(", ", operands.stream().map(TranspilerNode::description).toList()));
    }
}
