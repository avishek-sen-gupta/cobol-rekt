package com.mojo.algorithms.domain;

import java.util.Set;

public record ClassifiedEdges<E>(Set<E> treeEdges, Set<E> backEdges, Set<E> forwardEdges, Set<E> crossEdges) {
}
