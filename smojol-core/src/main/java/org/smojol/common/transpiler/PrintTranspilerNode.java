package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

import java.util.List;

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
}
