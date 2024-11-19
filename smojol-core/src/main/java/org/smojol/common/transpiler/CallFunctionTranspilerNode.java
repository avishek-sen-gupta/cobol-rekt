package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;

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
