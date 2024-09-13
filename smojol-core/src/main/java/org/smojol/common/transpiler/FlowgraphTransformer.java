package org.smojol.common.transpiler;

import org.jgrapht.Graph;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;

public class FlowgraphTransformer<V, E> {
    private final BiFunction<V, V, E> buildEdge;
    private final Graph<V, E> graph;
    private List<V> affectedNodes = new ArrayList<>();

    public FlowgraphTransformer(BiFunction<V, V, E> buildEdge, Graph<V, E> graph) {
        this.buildEdge = buildEdge;
        this.graph = graph;
    }

    public void applyT1(V node) {
        List<E> selfEdges = selfLoops(node, graph);
        selfEdges.forEach(graph::removeEdge);
    }

    public void applyT2(V node) {
        List<V> incomingVertices = incomingVertices(node);
        if (incomingVertices.stream().distinct().count() != 1) return;
        merge(incomingVertices.getFirst(), node);
        affectedNodes.add(node);
    }

    private List<E> selfLoops(V node, Graph<V, E> graph) {
        return graph.outgoingEdgesOf(node).stream().filter(e -> graph.getEdgeTarget(e).equals(node)).toList();
    }

    public void merge(V predecessor, V successor) {
        List<V> outgoingVertices = outgoingVertices(successor);
        outgoingVertices.forEach(ov -> graph.addEdge(predecessor, ov, buildEdge.apply(predecessor, ov)));
        graph.removeVertex(successor);
    }

    private List<V> outgoingVertices(V node) {
        return graph.outgoingEdgesOf(node).stream().map(graph::getEdgeTarget).toList();
    }

    private List<V> incomingVertices(V node) {
        return graph.incomingEdgesOf(node).stream().map(graph::getEdgeSource).toList();
    }

    public void addVertex(V vertex) {
        graph.addVertex(vertex);
    }

    public void addEdge(V from, V to) {
        graph.addEdge(from, to, buildEdge.apply(from, to));
    }

    public void reduce() {
        graph.vertexSet().forEach(this::applyT1);
        do {
            affectedNodes = new ArrayList<>();
            List<V> nodes = new ArrayList<>(graph.vertexSet());
            nodes.forEach(this::applyT2);
        } while (!affectedNodes.isEmpty());
    }
}
