package com.mojo.algorithms.domain;

import org.jgrapht.Graph;

import java.util.List;

public record FlowgraphReductionResult<V, E>(Graph<V, E> limitFlowGraph, List<String> evolutions, boolean isReducible) {
}
