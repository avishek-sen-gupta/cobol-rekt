package com.mojo.algorithms.domain;

import com.google.common.collect.Sets;
import com.mojo.algorithms.task.NodeDFSStatistics;
import com.mojo.algorithms.domain.exception.CyclicGraphException;
import com.mojo.algorithms.id.Identifiable;
import org.jgrapht.Graph;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

public record DepthFirstSpanningTree<V extends Identifiable, E>(List<V> preOrder, V sourceGraphRoot,
                                                                Graph<V, E> sourceGraph,
                                                                Map<V, NodeDFSStatistics> nodeStats,
                                                                Map<Integer, Set<V>> depthToNodeMap,
                                                                Graph<V, E> spanningTree) {

    public List<V> preOrdered() {
        return preOrder;
    }

    public List<V> postOrdered() {
        return preOrder.reversed();
    }

    public boolean isAncestorOf(V possibleAncestor, V possibleDescendant) {
        int ancestorClockTimeStart = nodeStats.get(possibleAncestor).discoveryStartTime();
        int ancestorClockTimeEnd = nodeStats.get(possibleAncestor).discoveryEndTime();
        int descendantClockTimeStart = nodeStats.get(possibleDescendant).discoveryStartTime();
        int descendantClockTimeEnd = nodeStats.get(possibleDescendant).discoveryEndTime();
        return ancestorClockTimeStart < descendantClockTimeStart && ancestorClockTimeEnd > descendantClockTimeEnd;
    }

    public ClassifiedEdges<E> classifiedEdges() {
        Set<E> treeEdges = spanningTree.edgeSet();
        Set<E> sourceGraphEdges = sourceGraph.edgeSet();
        Set<E> nonTreeEdges = Sets.difference(sourceGraphEdges, treeEdges);
        Set<E> backEdges = nonTreeEdges.stream().filter(e -> sourceGraph.getEdgeTarget(e) == sourceGraph.getEdgeSource(e)
                || isAncestorOf(sourceGraph.getEdgeTarget(e), sourceGraph.getEdgeSource(e))).collect(Collectors.toUnmodifiableSet());
        Set<E> forwardEdges = nonTreeEdges.stream().filter(e -> isAncestorOf(sourceGraph.getEdgeSource(e), sourceGraph.getEdgeTarget(e))).collect(Collectors.toUnmodifiableSet());
        Set<E> crossEdges = nonTreeEdges.stream().filter(e -> !isAncestorOf(sourceGraph.getEdgeSource(e), sourceGraph.getEdgeTarget(e))
                && !isAncestorOf(sourceGraph.getEdgeTarget(e), sourceGraph.getEdgeSource(e))).collect(Collectors.toUnmodifiableSet());

        return new ClassifiedEdges<>(treeEdges, backEdges, forwardEdges, crossEdges);
    }

    public int treeDepth(V v) {
        return nodeStats.get(v).treeDepth();
    }

    public List<V> topologicallyOrdered() {
        if (!classifiedEdges().backEdges().isEmpty()) throw new CyclicGraphException();
        return nodeStats.entrySet().stream().sorted((o1, o2) -> {
            int lhsDiscoveryEndTime = o1.getValue().discoveryEndTime();
            int rhsDiscoveryEndTime = o2.getValue().discoveryEndTime();
            if (lhsDiscoveryEndTime == rhsDiscoveryEndTime) return 0;
            return lhsDiscoveryEndTime < rhsDiscoveryEndTime ? 1 : -1;
        }).map(Entry::getKey).toList();
    }
}
