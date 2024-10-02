package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.id.Identifiable;

import java.util.*;

public class DepthFirstTraversalLabelTask<V extends Identifiable, E> {
    public static final String DFS_NUM = "DFS_NUM";
    private final List<GraphNodeLike> depthFirstSpanningTreeOrder = new ArrayList<>();
    private final GraphNodeLike root;
    private final Graph<GraphNodeLike, DefaultEdge> graph;
    private int currentDfsNumber;

    public DepthFirstTraversalLabelTask(GraphNodeLike root, Graph<GraphNodeLike, DefaultEdge> graph) {
        this(root, graph, 0);
    }

    public DepthFirstTraversalLabelTask(GraphNodeLike root, Graph<GraphNodeLike, DefaultEdge> graph, int startNumber) {
        this.root = root;
        this.graph = graph;
        this.currentDfsNumber = startNumber;
    }

    public DepthFirstTraversalLabelTask(Graph<V, E> sourceGraph, V root) {
        this(new CodeGraphNode(root.id()), unorderedGraph(sourceGraph), 0);
    }

    private static <V extends Identifiable, E> Graph<GraphNodeLike, DefaultEdge> unorderedGraph(Graph<V, E> graph) {
        DefaultDirectedGraph<GraphNodeLike, DefaultEdge> graphForDominators = new DefaultDirectedGraph<>(DefaultEdge.class);
        Graph<V, E> jgraph = graph;
        List<CodeGraphNode> xvs = jgraph.vertexSet().stream().map(v -> new CodeGraphNode(v.id())).toList();
        xvs.forEach(graphForDominators::addVertex);
        jgraph.edgeSet().forEach(edge -> graphForDominators.addEdge(new CodeGraphNode(jgraph.getEdgeSource(edge).id()), new CodeGraphNode(jgraph.getEdgeTarget(edge).id())));
        return graphForDominators;
    }

    public DepthFirstSpanningTree run() {
        run(root);
        return new DepthFirstSpanningTree(root, graph, depthFirstSpanningTreeOrder);
    }

    private void run(GraphNodeLike current) {
        current.setProperty(DFS_NUM, currentDfsNumber);
        depthFirstSpanningTreeOrder.add(current);
        currentDfsNumber++;
        List<GraphNodeLike> unvisitedChildren = graph.outgoingEdgesOf(current).stream()
                .map(graph::getEdgeTarget).toList();
        for (GraphNodeLike child : unvisitedChildren) {
            if (child.getProperty(DFS_NUM, Integer.class) != null) continue;
            run(child);
        }
    }

    public int max() {
        return currentDfsNumber;
    }
}
