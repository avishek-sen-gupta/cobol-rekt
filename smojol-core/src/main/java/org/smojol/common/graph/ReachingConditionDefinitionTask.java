package org.smojol.common.graph;

import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.transpiler.*;
import org.smojol.common.vm.type.TypedRecord;

import java.util.Set;
import java.util.stream.Collectors;

public class ReachingConditionDefinitionTask<V extends TranspilerInstruction, E> {
    private final GraphSlice<V, E> slice;

    public ReachingConditionDefinitionTask(GraphSlice<V, E> slice) {
        this.slice = slice;
    }

    public Set<TranspilerNode> run() {
        return slice.allPaths().stream().map(path -> path.getVertexList().stream().filter(v -> v.sentinel() == CodeSentinelType.BODY && v.ref() instanceof IfTranspilerNode)
                .map(obj -> ((IfTranspilerNode) obj.ref()).getCondition())
                .reduce(new PrimitiveValueTranspilerNode(TypedRecord.TRUE), AndTranspilerNode::new)).collect(Collectors.toUnmodifiableSet());
    }
}
