package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.type.TypedRecord;

import java.util.List;

public class FunctionCallNode extends TranspilerNode {
    private final String functionName;
    private final List<TranspilerNode> arguments;

    public FunctionCallNode(String functionName, List<TranspilerNode> arguments) {
        this.functionName = functionName;
        this.arguments = arguments;
    }

    @Override
    public String description() {
        return String.format("%s(%s)", functionName, String.join(", ", arguments.stream().map(TranspilerNode::description).toList()));
    }
}
