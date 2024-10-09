package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.smojol.common.id.Identifiable;

import java.util.Map;
import java.util.Set;

public record DJTree<V extends Identifiable, E>(V root, Graph<V, E> graph, Map<Integer, Set<V>> dominatorLevels) {
}
