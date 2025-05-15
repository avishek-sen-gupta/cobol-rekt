package com.mojo.algorithms.task;

import org.jgrapht.Graph;
import com.mojo.algorithms.id.Identifiable;

import java.util.HashSet;
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

    public Set<V> run() {
        V excludedNode = graph.getEdgeTarget(backEdge);
        Set<V> discoveredNodes = new HashSet<>(List.of(excludedNode));
        V root = graph.getEdgeSource(backEdge);
        run(root, discoveredNodes, graph);
        return discoveredNodes;
    }

    private void run(V current, Set<V> discoveredNodes, Graph<V, E> graph) {
        discoveredNodes.add(current);
        Set<E> incomingEdges = graph.incomingEdgesOf(current);
        System.out.printf("Incoming edges of %s: %s%n", current.id(), String.join(",", incomingEdges.stream().map(e -> graph.getEdgeSource(e).id()).toList()));
        for (E incomingEdge : incomingEdges) {
            V child = this.graph.getEdgeSource(incomingEdge);
            if (discoveredNodes.contains(child)) continue;
            run(child, discoveredNodes, graph);
        }
    }
}
