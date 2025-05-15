package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.TranspilerNode;
import lombok.Getter;
import com.mojo.algorithms.domain.SemanticCategory;

import java.util.Collection;
import java.util.List;

@Getter
public class CallFunctionTranspilerNode extends TranspilerNode {
    private final String functionName;
    private final List<TranspilerNode> arguments;

    public CallFunctionTranspilerNode(String functionName, List<TranspilerNode> arguments) {
        super(ImmutableList.of(SemanticCategory.FUNCTION));
        this.functionName = functionName;
        this.arguments = arguments;
    }

    @Override
    public String description() {
        return String.format("%s(%s)", functionName, String.join(", ", arguments.stream().map(TranspilerNode::description).toList()));
    }

    @Override
    public Collection<TranspilerNode> internalElements() {
        return arguments;
    }
}
