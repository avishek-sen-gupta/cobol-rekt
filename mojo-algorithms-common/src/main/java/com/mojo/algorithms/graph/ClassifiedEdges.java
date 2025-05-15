package com.mojo.algorithms.graph;

import java.util.Set;

public record ClassifiedEdges<E>(Set<E> treeEdges, Set<E> backEdges, Set<E> forwardEdges, Set<E> crossEdges) {
}
