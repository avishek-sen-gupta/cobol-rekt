package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;

import java.util.ArrayList;
import java.util.List;

public class DepthFirstTraversalLabelTask {
    public static final String DFS_NUM = "DFS_NUM";
    private final List<GraphNodeLike> depthFirstSpanningTreeOrder = new ArrayList<>();
    private final GraphNodeLike root;
    private final Graph<GraphNodeLike, DefaultEdge> graph;
    private int currentDfsNumber = 0;

    public DepthFirstTraversalLabelTask(GraphNodeLike root, Graph<GraphNodeLike, DefaultEdge> graph) {
        this(root, graph, 0);
    }

    public DepthFirstTraversalLabelTask(GraphNodeLike root, Graph<GraphNodeLike, DefaultEdge> graph, int startNumber) {
        this.root = root;
        this.graph = graph;
        this.currentDfsNumber = startNumber;
    }

    public void run() {
        run(root);
    }

    public void run(GraphNodeLike current) {
        current.setProperty(DFS_NUM, currentDfsNumber);
        depthFirstSpanningTreeOrder.add(current);
        currentDfsNumber++;
        List<GraphNodeLike> unvisitedChildren = graph.outgoingEdgesOf(current).stream()
                .map(graph::getEdgeTarget).toList();
        for (GraphNodeLike child : unvisitedChildren) {
            if (child.getProperty("DFS_NUM", Integer.class) != null) continue;
            run(child);
        }
    }

    public int max() {
        return currentDfsNumber;
    }

    public List<GraphNodeLike> preOrder() {
        return depthFirstSpanningTreeOrder;
    }

    public List<GraphNodeLike> postOrder() {
        return depthFirstSpanningTreeOrder.reversed();
    }
}
