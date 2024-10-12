package org.smojol.common.graph;

import org.jgrapht.GraphPath;

import java.util.List;

public record GraphSlice<V, E>(List<GraphPath<V, E>> allPaths) {
}
