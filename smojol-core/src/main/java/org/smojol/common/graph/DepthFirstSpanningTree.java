package org.smojol.common.graph;

import org.jgrapht.Graph;

import java.util.List;

public record DepthFirstSpanningTree(GraphNodeLike root,
                                     Graph<GraphNodeLike, org.jgrapht.graph.DefaultEdge> graph,
                                     List<GraphNodeLike> depthFirstSpanningTreeOrder) {

    public List<GraphNodeLike> preOrder() {
        return depthFirstSpanningTreeOrder;
    }

    public List<GraphNodeLike> postOrder() {
        return depthFirstSpanningTreeOrder.reversed();
    }
}
