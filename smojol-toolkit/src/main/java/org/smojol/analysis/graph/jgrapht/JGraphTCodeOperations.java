package org.smojol.analysis.graph.jgrapht;

import org.jgrapht.Graph;
import org.smojol.analysis.graph.graphml.TypedCodeVertex;
import org.smojol.analysis.graph.graphml.TypedGraphEdge;
import org.smojol.analysis.graph.graphml.TypedGraphVertex;
import org.smojol.common.flowchart.FlowNode;

public class JGraphTCodeOperations {
    private final Graph<TypedGraphVertex, TypedGraphEdge> graph;

    public JGraphTCodeOperations(Graph<TypedGraphVertex, TypedGraphEdge> graph) {
        this.graph = graph;
    }

    public boolean addNode(FlowNode node) {
        return graph.addVertex(new TypedCodeVertex(node));
    }

    public boolean connect(FlowNode from, FlowNode to, String edgeType) {
        TypedCodeVertex vFrom = new TypedCodeVertex(from);
        TypedCodeVertex vTo = new TypedCodeVertex(to);
        if (!graph.containsVertex(vFrom)) graph.addVertex(vFrom);
        if (!graph.containsVertex(vTo)) graph.addVertex(vTo);
        return graph.addEdge(vFrom, vTo, new TypedGraphEdge(edgeType));
    }
}
