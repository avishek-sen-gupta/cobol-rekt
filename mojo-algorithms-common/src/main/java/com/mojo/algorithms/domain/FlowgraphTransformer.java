package com.mojo.algorithms.domain;

import org.jgrapht.Graph;
import com.mojo.algorithms.id.Identifiable;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.logging.Logger;

public class FlowgraphTransformer<V extends Identifiable, E> {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(FlowgraphTransformer.class.getName());
    private final BiFunction<V, V, E> buildEdge;
    private final Graph<V, E> graph;
    private final Function<V, Boolean> isRoot;
    private final List<String> evolutions = new ArrayList<>();

    public FlowgraphTransformer(Graph<V, E> graph, BiFunction<V, V, E> buildEdge, Function<V, Boolean> isRoot) {
        this.buildEdge = buildEdge;
        this.graph = graph;
        this.isRoot = isRoot;
    }

    public void applyT1(V node, List<V> affectedNodes) {
        List<E> selfEdges = selfLoops(node, graph);
        if (selfEdges.isEmpty()) return;
        selfEdges.forEach(graph::removeEdge);
        affectedNodes.add(node);
        LOGGER.finer("Removed self-loop from " + node.label() + "...");
    }

    public void applyT2(V node, List<V> affectedNodes) {
        List<V> incomingVertices = incomingVertices(node);
        if (isRoot.apply(node)) return;
        Set<V> incomingVertexSet = new HashSet<>(incomingVertices);
        if (incomingVertexSet.isEmpty())
            throw new RuntimeException("This cannot happen! " + node.label());
        if (incomingVertexSet.size() != 1) return;
        merge(incomingVertices.getFirst(), node);
        affectedNodes.add(node);
//        evolutions.add(new MermaidGraph<V, E>().draw(sourceGraph));
    }

    private List<E> selfLoops(V node, Graph<V, E> graph) {
        return graph.outgoingEdgesOf(node).stream().filter(e -> graph.getEdgeTarget(e).equals(node)).toList();
    }

    public void merge(V predecessor, V successor) {
        LOGGER.finer(String.format("Merging %s into %s...", successor.label(), predecessor.label()));
        List<V> outgoingVertices = outgoingVertices(successor);
        outgoingVertices.stream().distinct().forEach(ov -> graph.addEdge(predecessor, ov, buildEdge.apply(predecessor, ov)));
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

    public FlowgraphReductionResult<V, E> reduce() {
        evolutions.add(new MermaidGraph<V, E>().draw(graph));
        List<V> affectedNodes = new ArrayList<>();
        do {
            affectedNodes.clear();
            List<V> nodesBeforeT1 = new ArrayList<>(graph.vertexSet());
            nodesBeforeT1.forEach(node -> applyT1(node, affectedNodes));
            List<V> nodesBeforeT2 = new ArrayList<>(graph.vertexSet());
            nodesBeforeT2.forEach(node -> applyT2(node, affectedNodes));
            evolutions.add(new MermaidGraph<V, E>().draw(graph));
        } while (!affectedNodes.isEmpty());

        return new FlowgraphReductionResult<>(graph, evolutions, graph.vertexSet().size() == 1);
    }
}
