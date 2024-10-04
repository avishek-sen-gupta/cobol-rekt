package org.smojol.common.graph;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.id.Identifiable;

import java.util.*;
import java.util.stream.Collectors;

public class DepthFirstTraversalLabelTask<V extends Identifiable, E> {
    public static final String DFS_DISCOVERY_START = "DFS_DISCOVERY_START";
    public static final String DFS_DISCOVERY_FINISH = "DFS_DISCOVERY_FINISH";
    private final List<CodeGraphNode<V>> depthFirstSpanningTreeOrder = new ArrayList<>();
    private final V sourceGraphRoot;
    private final Graph<CodeGraphNode<V>, DefaultEdge> labelledGraph;
    private final Graph<V, E> sourceGraph;
    private int currentDfsClock;

    public DepthFirstTraversalLabelTask(V root, Graph<V, E> sourceGraph) {
        this(root, sourceGraph, 0);
    }

    public DepthFirstTraversalLabelTask(V sourceGraphRoot, Graph<V, E> sourceGraph, int startNumber) {
        this.sourceGraphRoot = sourceGraphRoot;
        this.labelledGraph = unorderedGraph(sourceGraph);
        this.sourceGraph = sourceGraph;
        this.currentDfsClock = startNumber;
    }

    private static <V extends Identifiable, E> Graph<CodeGraphNode<V>, DefaultEdge> unorderedGraph(Graph<V, E> sourceGraph) {
        Graph<CodeGraphNode<V>, DefaultEdge> graphForDominators = new DefaultDirectedGraph<>(DefaultEdge.class);
        Map<V, CodeGraphNode<V>> sourceToDFSNodeMap = sourceGraph.vertexSet().stream().collect(Collectors.toMap(xv -> xv, CodeGraphNode::new));
        sourceToDFSNodeMap.forEach((k, v) -> graphForDominators.addVertex(v));
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
        Map<V, Pair<Integer, Integer>> clockTimes = labelledGraph.vertexSet().stream().collect(Collectors.toMap(CodeGraphNode::getOriginalNode,
                v -> ImmutablePair.of(v.getProperty(DFS_DISCOVERY_START, Integer.class), v.getProperty(DFS_DISCOVERY_FINISH, Integer.class))));
        return new DepthFirstSpanningTree<>(dfsOrderedOriginalNodes, sourceGraphRoot, sourceGraph, labelledGraph, labelledGraphRoot, clockTimes);
    }

    private void run(CodeGraphNode<V> current) {
        current.setProperty(DFS_DISCOVERY_START, currentDfsClock++);
        depthFirstSpanningTreeOrder.add(current);
//        currentDfsClock++;
        List<CodeGraphNode<V>> unvisitedChildren = labelledGraph.outgoingEdgesOf(current).stream()
                .map(labelledGraph::getEdgeTarget).toList();
        for (CodeGraphNode<V> child : unvisitedChildren) {
            if (child.getProperty(DFS_DISCOVERY_START, Integer.class) != null) continue;
            run(child);
        }
        current.setProperty(DFS_DISCOVERY_FINISH, currentDfsClock++);
    }

    public int currentClock() {
        return currentDfsClock;
    }
}
