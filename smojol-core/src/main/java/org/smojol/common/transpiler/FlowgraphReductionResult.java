package org.smojol.common.transpiler;

import org.jgrapht.Graph;

import java.util.List;

public record FlowgraphReductionResult<V, E>(Graph<V, E> graph, List<String> evolutions, boolean isReducible) {
}
