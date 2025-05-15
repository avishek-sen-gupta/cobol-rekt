package com.mojo.algorithms.domain;

import org.jgrapht.Graph;
import com.mojo.algorithms.id.Identifiable;

import java.util.Map;
import java.util.Set;

public record DJTree<V extends Identifiable, E>(V root, Graph<V, E> graph, Map<Integer, Set<V>> dominatorLevels) {
}
