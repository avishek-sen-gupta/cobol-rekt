package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;

import java.util.List;

public class DepthFirstTraversalLabelTask {
    private final GraphNodeLike root;
    private final Graph<GraphNodeLike, DefaultEdge> graph;
    private int currentDfsNumber = 0;

    public DepthFirstTraversalLabelTask(GraphNodeLike root, Graph<GraphNodeLike, DefaultEdge> graph) {
        this.root = root;
        this.graph = graph;
    }

    public void run() {
        run(root);
    }

    public void run(GraphNodeLike current) {
        current.setProperty("DFS_NUM", currentDfsNumber++);
        List<GraphNodeLike> unvisitedChildren = graph.outgoingEdgesOf(current).stream()
                .map(graph::getEdgeTarget)
                .filter(n -> n.getProperty("DFS_NUM", Integer.class) == null).toList();
        unvisitedChildren.forEach(this::run);
    }

    public int max() {
        return currentDfsNumber;
    }
}
