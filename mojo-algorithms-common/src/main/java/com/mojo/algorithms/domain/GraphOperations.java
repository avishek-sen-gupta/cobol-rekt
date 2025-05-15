package com.mojo.algorithms.domain;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import com.mojo.algorithms.id.Identifiable;

import java.util.function.Function;

public class GraphOperations {
    public static <V extends Identifiable, E> Graph<V, E> cloneGraph(Graph<V, E> graph, Class<E> edgeClass, Function<E, E> cloneEdge) {
        Graph<V, E> clonedGraph = new DefaultDirectedGraph<>(edgeClass);
        graph.vertexSet().forEach(clonedGraph::addVertex);
        graph.edgeSet().forEach(edge -> clonedGraph.addEdge(graph.getEdgeSource(edge), graph.getEdgeTarget(edge), edge));
        return clonedGraph;
    }

    public static <V extends Identifiable, E> Graph<V, E> duplicateGraph(Graph<V, E> graph, Class<E> edgeClass, Function<E, E> cloneEdge) {
        Graph<V, E> clonedGraph = new DefaultDirectedGraph<>(edgeClass);
        graph.vertexSet().forEach(clonedGraph::addVertex);
        graph.edgeSet().forEach(edge -> clonedGraph.addEdge(graph.getEdgeSource(edge), graph.getEdgeTarget(edge), cloneEdge.apply(edge)));
        return clonedGraph;
    }
}
