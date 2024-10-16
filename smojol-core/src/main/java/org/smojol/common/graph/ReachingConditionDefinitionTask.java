package org.smojol.common.graph;

import com.google.common.collect.ImmutableMap;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerNode;

import java.util.Map;
import java.util.Set;

public class ReachingConditionDefinitionTask<V extends TranspilerInstruction, E> {
    public Map<V, Set<TranspilerNode>> run(GraphSlice<V, E> graphSlice) {
        return ImmutableMap.of();
    }
}
