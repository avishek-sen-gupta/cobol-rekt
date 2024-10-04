package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.id.Identifiable;

public record DJTree<V extends Identifiable>(V root, Graph<V, DefaultEdge> graph) {
}
