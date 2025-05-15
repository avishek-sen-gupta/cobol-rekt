package com.mojo.algorithms.graph;

import com.mojo.algorithms.id.Identifiable;
import org.jgrapht.Graph;

public record DominatorTree<V extends Identifiable, E>(V root, Graph<V, E> graph) {
}
