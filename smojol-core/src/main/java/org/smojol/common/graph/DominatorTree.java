package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.smojol.common.id.Identifiable;

public record DominatorTree<V extends Identifiable, E>(V root, Graph<V, E> graph) {
}
