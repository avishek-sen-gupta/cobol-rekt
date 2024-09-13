package org.smojol.common.transpiler;

import org.jgrapht.Graph;
import org.smojol.common.flowchart.MermaidGraph;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;

public class FlowgraphTransformer<V, E> {
    private final BiFunction<V, V, E> buildEdge;
    private final Graph<V, E> graph;
    private final List<String> graphs = new ArrayList<>();

    public FlowgraphTransformer(BiFunction<V, V, E> buildEdge, Graph<V, E> graph) {
        this.buildEdge = buildEdge;
        this.graph = graph;
    }

    public void applyT1(V node, List<V> affectedNodes) {
        List<E> selfEdges = selfLoops(node, graph);
        if (selfEdges.isEmpty()) return;
        selfEdges.forEach(graph::removeEdge);
        affectedNodes.add(node);
    }

    public void applyT2(V node, List<V> affectedNodes) {
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

    public List<String> reduce() {
        graphs.add(new MermaidGraph<V, E>().draw(graph));
        List<V> affectedNodes = new ArrayList<>();
        do {
            affectedNodes.clear();
            List<V> nodes = new ArrayList<>(graph.vertexSet());
            nodes.forEach(node -> applyT1(node, affectedNodes));
            graphs.add(new MermaidGraph<V, E>().draw(graph));
            nodes.forEach(node -> applyT2(node, affectedNodes));
            graphs.add(new MermaidGraph<V, E>().draw(graph));
        } while (!affectedNodes.isEmpty());

        return graphs;
    }
}
