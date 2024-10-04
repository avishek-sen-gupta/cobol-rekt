package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.id.Identifiable;

public record DominatorTree(Identifiable root, Graph<Identifiable, DefaultEdge> graph) {
}
