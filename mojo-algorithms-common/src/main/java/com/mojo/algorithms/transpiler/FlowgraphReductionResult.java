package com.mojo.algorithms.transpiler;

import org.jgrapht.Graph;

import java.util.List;

public record FlowgraphReductionResult<V, E>(Graph<V, E> limitFlowGraph, List<String> evolutions, boolean isReducible) {
}
