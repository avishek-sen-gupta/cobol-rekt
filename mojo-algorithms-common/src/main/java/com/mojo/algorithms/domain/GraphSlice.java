package com.mojo.algorithms.domain;

import org.jgrapht.Graph;
import org.jgrapht.GraphPath;

import java.util.List;

public record GraphSlice<V, E>(List<GraphPath<V, E>> allPaths, List<V> topologicallyOrderedVertices, Graph<V, E> sourceGraph) {
}
