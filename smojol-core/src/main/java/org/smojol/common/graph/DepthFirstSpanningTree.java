package org.smojol.common.graph;

import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.id.Identifiable;

import java.util.List;
import java.util.Map;

public record DepthFirstSpanningTree<V extends Identifiable, E>(List<V> depthFirstSpanningTreeOrder, V sourceGraphRoot,
                                                                Graph<V, E> sourceGraph,
                                                                Graph<CodeGraphNode<V>, DefaultEdge> dfsLabelledTree,
                                                                CodeGraphNode<V> dfsLabelledTreeRoot,
                                                                Map<V, Pair<Integer, Integer>> clockTimes) {

    public List<V> preOrder() {
        return depthFirstSpanningTreeOrder;
    }

    public List<V> postOrder() {
        return depthFirstSpanningTreeOrder.reversed();
    }

    public boolean isAncestorOf(V possibleAncestor, V possibleDescendant) {
        Pair<Integer, Integer> ancestorClockTimes = clockTimes.get(possibleAncestor);
        Pair<Integer, Integer> descendantClockTimes = clockTimes.get(possibleDescendant);
        return ancestorClockTimes.getLeft() < descendantClockTimes.getLeft() && ancestorClockTimes.getRight() > descendantClockTimes.getRight();
    }
}
