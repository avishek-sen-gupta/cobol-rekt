package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.smojol.common.id.Identifiable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/*
Based on "Constructing the natural loop of a back edge"
in Aho, Sethi, Ullman (Updated Second Edition) p. 665 (Algorithm 9.46)
in Aho, Sethi, Ullman (1986) p. 604 (Algorithm 10.1)
 */
public class NaturalLoopOfBackEdgeTask<V extends Identifiable, E> {
    private final E backEdge;
    private final Graph<V, E> graph;

    public NaturalLoopOfBackEdgeTask(E backEdge, Graph<V, E> graph) {
        this.backEdge = backEdge;
        this.graph = graph;
    }

    public List<V> run() {
        V excludedNode = graph.getEdgeTarget(backEdge);
        List<V> discoveredNodes = new ArrayList<>(List.of(excludedNode));
        V root = graph.getEdgeSource(backEdge);
        run(root, discoveredNodes, graph);
        return discoveredNodes;
    }

    private void run(V current, List<V> discoveredNodes, Graph<V, E> graph) {
        discoveredNodes.add(current);
        Set<E> incomingEdges = graph.incomingEdgesOf(current);
        for (E incomingEdge : incomingEdges) {
            V child = this.graph.getEdgeSource(incomingEdge);
            if (discoveredNodes.contains(child)) continue;
            run(child, discoveredNodes, graph);
        }
    }
}
