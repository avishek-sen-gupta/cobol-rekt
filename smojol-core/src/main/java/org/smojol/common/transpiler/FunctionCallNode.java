package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

import java.util.List;

public class FunctionCallNode extends TranspilerNode {
    private final String functionName;
    private final List<TranspilerNode> arguments;

    public FunctionCallNode(String functionName, List<TranspilerNode> arguments) {
        super(ImmutableList.of(SemanticCategory.FUNCTION));
        this.functionName = functionName;
        this.arguments = arguments;
    }

    @Override
    public String description() {
        return String.format("%s(%s)", functionName, String.join(", ", arguments.stream().map(TranspilerNode::description).toList()));
    }
}
