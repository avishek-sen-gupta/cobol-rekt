package org.smojol.common.graph;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.smojol.common.id.Identifiable;

import java.util.*;
import java.util.stream.Collectors;

public class DepthFirstTraversalLabelTask<V extends Identifiable, E> {
    private final List<V> depthFirstOrderedNodes = new ArrayList<>();
    private final V sourceGraphRoot;
    private final Graph<V, E> spanningTree;
    private final Graph<V, E> sourceGraph;
    private int dfsClock;
    private final Map<V, Pair<Integer, Integer>> discoveryTimes;

    public DepthFirstTraversalLabelTask(V root, Graph<V, E> sourceGraph, Class<E> edgeClass) {
        this(root, sourceGraph, 0, edgeClass);
    }

    public DepthFirstTraversalLabelTask(V sourceGraphRoot, Graph<V, E> sourceGraph, int startNumber, Class<E> edgeClass) {
        this.sourceGraphRoot = sourceGraphRoot;
        this.spanningTree = emptySpanningTree(sourceGraph, edgeClass);
        this.sourceGraph = sourceGraph;
        this.dfsClock = startNumber;
        discoveryTimes = sourceGraph.vertexSet().stream().collect(Collectors.toMap(v -> v, v -> ImmutablePair.of(-1, -1)));
    }

    private Graph<V, E> emptySpanningTree(Graph<V, E> sourceGraph, Class<E> edgeClass) {
        Graph<V, E> spanningTree = new DefaultDirectedGraph<>(edgeClass);
        sourceGraph.vertexSet().forEach(spanningTree::addVertex);
        return spanningTree;
    }

    public DepthFirstSpanningTree<V, E> run() {
        run(sourceGraphRoot);
        return new DepthFirstSpanningTree<>(depthFirstOrderedNodes, sourceGraphRoot, sourceGraph, discoveryTimes, spanningTree);
    }

    private void run(V current) {
        discoveryTimes.put(current, ImmutablePair.of(dfsClock++, -1));
        depthFirstOrderedNodes.add(current);
        List<V> children = sourceGraph.outgoingEdgesOf(current).stream()
                .map(sourceGraph::getEdgeTarget).toList();
        for (V child : children) {
            if (discoveryTimes.get(child).getLeft() != -1) continue;
            E existingEdge = sourceGraph.edgeSet().stream().filter(e -> sourceGraph.getEdgeSource(e) == current && sourceGraph.getEdgeTarget(e) == child).findFirst().get();
            spanningTree.addEdge(current, child, existingEdge);
            run(child);
        }
        discoveryTimes.put(current, ImmutablePair.of(discoveryTimes.get(current).getLeft(), dfsClock++));
    }

    public int currentClock() {
        return dfsClock;
    }
}
