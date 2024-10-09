package org.smojol.common.graph;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.smojol.common.id.Identifiable;

public class GraphOperations {
    public static <V extends Identifiable, E> Graph<V, E> cloneGraph(Graph<V, E> graph, Class<E> edgeClass) {
        Graph<V, E> clonedGraph = new DefaultDirectedGraph<>(edgeClass);
        graph.vertexSet().forEach(clonedGraph::addVertex);
        graph.edgeSet().forEach(edge -> clonedGraph.addEdge(graph.getEdgeSource(edge), graph.getEdgeTarget(edge), edge));
        return clonedGraph;
    }
}
