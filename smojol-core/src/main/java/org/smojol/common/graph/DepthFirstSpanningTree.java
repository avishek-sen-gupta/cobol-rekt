package org.smojol.common.graph;

import com.google.common.collect.Sets;
import org.jgrapht.Graph;
import org.smojol.common.id.Identifiable;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public record DepthFirstSpanningTree<V extends Identifiable, E>(List<V> depthFirstSpanningTreeOrder, V sourceGraphRoot,
                                                                Graph<V, E> sourceGraph,
                                                                Map<V, NodeDFSStatistics> nodeStats,
                                                                Map<Integer, Set<V>> depthToNodeMap,
                                                                Graph<V, E> spanningTree) {

    public List<V> preOrder() {
        return depthFirstSpanningTreeOrder;
    }

    public List<V> postOrder() {
        return depthFirstSpanningTreeOrder.reversed();
    }

    public boolean isAncestorOf(V possibleAncestor, V possibleDescendant) {
        int ancestorClockTimeStart = nodeStats.get(possibleAncestor).discoveryStartTime();
        int ancestorClockTimeEnd = nodeStats.get(possibleAncestor).discoveryEndTime();
        int descendantClockTimeStart = nodeStats.get(possibleDescendant).discoveryStartTime();
        int descendantClockTimeEnd = nodeStats.get(possibleDescendant).discoveryStartTime();
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
}
