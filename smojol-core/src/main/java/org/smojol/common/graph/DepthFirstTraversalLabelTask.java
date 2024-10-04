package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.id.Identifiable;

import java.util.*;

public class DepthFirstTraversalLabelTask<V extends Identifiable, E> {
    public static final String DFS_NUM = "DFS_NUM";
    private final List<CodeGraphNode<V>> depthFirstSpanningTreeOrder = new ArrayList<>();
    private final V sourceGraphRoot;
    private final Graph<CodeGraphNode<V>, DefaultEdge> labelledGraph;
    private final Graph<V, E> sourceGraph;
    private int currentDfsNumber;

    public DepthFirstTraversalLabelTask(V root, Graph<V, E> sourceGraph) {
        this(root, sourceGraph, 0);
    }

    public DepthFirstTraversalLabelTask(V sourceGraphRoot, Graph<V, E> sourceGraph, int startNumber) {
        this.sourceGraphRoot = sourceGraphRoot;
        this.labelledGraph = unorderedGraph(sourceGraph);
        this.sourceGraph = sourceGraph;
        this.currentDfsNumber = startNumber;
    }

    private static <V extends Identifiable, E> Graph<CodeGraphNode<V>, DefaultEdge> unorderedGraph(Graph<V, E> sourceGraph) {
        DefaultDirectedGraph<CodeGraphNode<V>, DefaultEdge> graphForDominators = new DefaultDirectedGraph<>(DefaultEdge.class);
        Map<V, CodeGraphNode<V>> sourceToDFSNodeMap = new HashMap<>();
//        List<CodeGraphNode<V>> xvs = sourceGraph.vertexSet().stream().map(CodeGraphNode::new).toList();
        sourceGraph.vertexSet().forEach(sourceVertex -> {
            CodeGraphNode<V> dfsLabelledVertex = new CodeGraphNode<>(sourceVertex);
            graphForDominators.addVertex(dfsLabelledVertex);
            sourceToDFSNodeMap.put(sourceVertex, dfsLabelledVertex);
        });

//        jgraph.edgeSet().forEach(edge -> graphForDominators.addEdge(new CodeGraphNode<>(jgraph.getEdgeSource(edge)), new CodeGraphNode<>(jgraph.getEdgeTarget(edge))));
//        sourceGraph.edgeSet().forEach(edge -> graphForDominators.addEdge(find(sourceGraph.getEdgeSource(edge), xvs), find(sourceGraph.getEdgeTarget(edge), xvs)));
        sourceGraph.edgeSet().forEach(edge -> graphForDominators.addEdge(sourceToDFSNodeMap.get(sourceGraph.getEdgeSource(edge)), sourceToDFSNodeMap.get(sourceGraph.getEdgeTarget(edge))));
        return graphForDominators;
    }

    private static <V extends Identifiable> CodeGraphNode<V> find(V node, Set<CodeGraphNode<V>> allNodes) {
        return allNodes.stream().filter(cgn -> cgn.getOriginalNode() == node).findFirst().orElse(null);
    }

    public DepthFirstSpanningTree<V, E> run() {
        CodeGraphNode<V> labelledGraphRoot = find(sourceGraphRoot, labelledGraph.vertexSet());
        run(labelledGraphRoot);
        List<V> dfsOrderedOriginalNodes = depthFirstSpanningTreeOrder.stream().map(CodeGraphNode::getOriginalNode).toList();
        return new DepthFirstSpanningTree<>(dfsOrderedOriginalNodes, sourceGraphRoot, sourceGraph, labelledGraph, labelledGraphRoot);
    }

    private void run(CodeGraphNode<V> current) {
        current.setProperty(DFS_NUM, currentDfsNumber);
        depthFirstSpanningTreeOrder.add(current);
        currentDfsNumber++;
        List<CodeGraphNode<V>> unvisitedChildren = labelledGraph.outgoingEdgesOf(current).stream()
                .map(labelledGraph::getEdgeTarget).toList();
        for (CodeGraphNode<V> child : unvisitedChildren) {
            if (child.getProperty(DFS_NUM, Integer.class) != null) continue;
            run(child);
        }
    }

    public int max() {
        return currentDfsNumber;
    }
}
