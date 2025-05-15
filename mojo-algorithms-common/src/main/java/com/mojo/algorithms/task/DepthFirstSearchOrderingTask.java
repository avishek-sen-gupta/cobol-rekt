package com.mojo.algorithms.task;

import com.mojo.algorithms.domain.DepthFirstSpanningTree;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import com.mojo.algorithms.id.Identifiable;

import java.util.*;
import java.util.stream.Collectors;

public class DepthFirstSearchOrderingTask<V extends Identifiable, E> {
    private final List<V> depthFirstOrderedNodes = new ArrayList<>();
    private final V sourceGraphRoot;
    private final Graph<V, E> spanningTree;
    private final Graph<V, E> sourceGraph;
    private int dfsClock;
    private final Map<V, NodeDFSStatistics> nodeStats;

    public DepthFirstSearchOrderingTask(V root, Graph<V, E> sourceGraph, Class<E> edgeClass) {
        this(root, sourceGraph, 0, edgeClass);
    }

    public DepthFirstSearchOrderingTask(V sourceGraphRoot, Graph<V, E> sourceGraph, int startNumber, Class<E> edgeClass) {
        this.sourceGraphRoot = sourceGraphRoot;
        this.spanningTree = emptySpanningTree(sourceGraph, edgeClass);
        this.sourceGraph = sourceGraph;
        this.dfsClock = startNumber;
        nodeStats = sourceGraph.vertexSet().stream().collect(Collectors.toMap(v -> v, v -> new NodeDFSStatistics(-1, -1, -1)));
    }

    private Graph<V, E> emptySpanningTree(Graph<V, E> sourceGraph, Class<E> edgeClass) {
        Graph<V, E> spanningTree = new DefaultDirectedGraph<>(edgeClass);
        sourceGraph.vertexSet().forEach(spanningTree::addVertex);
        return spanningTree;
    }

    public DepthFirstSpanningTree<V, E> run() {
        run(sourceGraphRoot, 0);
        Map<Integer, Set<V>> depthToNodeMap = new HashMap<>();
        nodeStats.values().forEach(v -> depthToNodeMap.put(v.treeDepth(), new HashSet<>()));
        nodeStats.forEach((key, value) -> depthToNodeMap.get(value.treeDepth()).add(key));
        return new DepthFirstSpanningTree<>(depthFirstOrderedNodes, sourceGraphRoot, sourceGraph, nodeStats, depthToNodeMap, spanningTree);
    }

    private void run(V current, int treeDepth) {
        nodeStats.put(current, new NodeDFSStatistics(treeDepth, dfsClock++, -1));
        depthFirstOrderedNodes.add(current);
        Set<E> outgoingEdges = sourceGraph.outgoingEdgesOf(current);
        for (E outgoingEdge : outgoingEdges) {
            V child = sourceGraph.getEdgeTarget(outgoingEdge);
            if (nodeStats.get(child).isInitialised()) continue;
            spanningTree.addEdge(current, child, outgoingEdge);
            run(child, treeDepth + 1);
        }
        nodeStats.put(current, new NodeDFSStatistics(treeDepth, nodeStats.get(current).discoveryStartTime(), dfsClock++));
    }

    public int currentClock() {
        return dfsClock;
    }
}
