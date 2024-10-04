package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.smojol.common.id.Identifiable;

import java.util.List;

public record DepthFirstSpanningTree<V extends Identifiable, E>(V root,
                                     Graph<V, E> graph,
                                     List<V> depthFirstSpanningTreeOrder) {

    public List<V> preOrder() {
        return depthFirstSpanningTreeOrder;
    }

    public List<V> postOrder() {
        return depthFirstSpanningTreeOrder.reversed();
    }
}
