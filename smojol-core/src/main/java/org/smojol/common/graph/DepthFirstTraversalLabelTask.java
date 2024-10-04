package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.id.Identifiable;

import java.util.*;

public class DepthFirstTraversalLabelTask<V extends Identifiable, E> {
    public static final String DFS_NUM = "DFS_NUM";
    private final List<CodeGraphNode<V>> depthFirstSpanningTreeOrder = new ArrayList<>();
    private final CodeGraphNode<V> root;
    private final V sourceGraphRoot;
    private final Graph<CodeGraphNode<V>, DefaultEdge> graph;
    private final Graph<V, E> sourceGraph;
    private int currentDfsNumber;

//    public DepthFirstTraversalLabelTask(CodeGraphNode root, Graph<CodeGraphNode, DefaultEdge> graph) {
//        this(root, graph, 0);
//    }
//
//    public DepthFirstTraversalLabelTask(CodeGraphNode root, Graph<CodeGraphNode, DefaultEdge> graph, int startNumber) {
//        this.root = root;
//        this.graph = graph;
//        this.currentDfsNumber = startNumber;
//    }

    public DepthFirstTraversalLabelTask(V root, Graph<V, E> sourceGraph) {
        this(root, sourceGraph, 0);
    }

    public DepthFirstTraversalLabelTask(V root, Graph<V, E> sourceGraph, int startNumber) {
        this.sourceGraphRoot = root;
        this.root = new CodeGraphNode<V>(root);
        this.graph = unorderedGraph(sourceGraph);
        this.sourceGraph = sourceGraph;
        this.currentDfsNumber = startNumber;
    }

    private static <V extends Identifiable, E> Graph<CodeGraphNode<V>, DefaultEdge> unorderedGraph(Graph<V, E> graph) {
        DefaultDirectedGraph<CodeGraphNode<V>, DefaultEdge> graphForDominators = new DefaultDirectedGraph<>(DefaultEdge.class);
        Graph<V, E> jgraph = graph;
        List<CodeGraphNode<V>> xvs = jgraph.vertexSet().stream().map(CodeGraphNode::new).toList();
        xvs.forEach(graphForDominators::addVertex);
        jgraph.edgeSet().forEach(edge -> graphForDominators.addEdge(new CodeGraphNode<V>(jgraph.getEdgeSource(edge)), new CodeGraphNode<V>(jgraph.getEdgeTarget(edge))));
        return graphForDominators;
    }

    public DepthFirstSpanningTree<V, E> run() {
        run(root);
        List<V> dfsOrderedOriginalNodes = depthFirstSpanningTreeOrder.stream().map(CodeGraphNode::getOriginalNode).toList();
        return new DepthFirstSpanningTree<>(sourceGraphRoot, sourceGraph, dfsOrderedOriginalNodes);
    }

    private void run(CodeGraphNode<V> current) {
        current.setProperty(DFS_NUM, currentDfsNumber);
        depthFirstSpanningTreeOrder.add(current);
        currentDfsNumber++;
        List<CodeGraphNode<V>> unvisitedChildren = graph.outgoingEdgesOf(current).stream()
                .map(graph::getEdgeTarget).toList();
        for (CodeGraphNode<V> child : unvisitedChildren) {
            if (child.getProperty(DFS_NUM, Integer.class) != null) continue;
            run(child);
        }
    }

    public int max() {
        return currentDfsNumber;
    }
}
