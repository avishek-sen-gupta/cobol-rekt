package org.smojol.toolkit.analysis.graph.jgrapht;

import org.jgrapht.Graph;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphEdge;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphVertex;
import org.smojol.common.ast.FlowNode;

public class JGraphTCodeOperations {
    private final Graph<TypedGraphVertex, TypedGraphEdge> graph;
    private final NodeSpecBuilder qualifier;

    public JGraphTCodeOperations(Graph<TypedGraphVertex, TypedGraphEdge> graph, NodeSpecBuilder qualifier) {
        this.graph = graph;
        this.qualifier = qualifier;
    }

    public boolean addNode(FlowNode node) {
        return graph.addVertex(qualifier.newCodeVertex(node));
    }

    public boolean connect(FlowNode from, FlowNode to, String edgeType) {
        TypedGraphVertex vFrom = qualifier.newCodeVertex(from);
        TypedGraphVertex vTo = qualifier.newCodeVertex(to);
        if (!graph.containsVertex(vFrom)) graph.addVertex(vFrom);
        if (!graph.containsVertex(vTo)) graph.addVertex(vTo);
        return graph.addEdge(vFrom, vTo, qualifier.newEdge(edgeType));
    }
}