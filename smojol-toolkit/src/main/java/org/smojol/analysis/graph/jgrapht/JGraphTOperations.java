package org.smojol.analysis.graph.jgrapht;

import org.jgrapht.Graph;
import org.smojol.analysis.graph.graphml.TypedGraphMLEdge;
import org.smojol.common.flowchart.FlowNode;

public class JGraphTOperations {
    private final Graph<FlowNode, TypedGraphMLEdge> graph;

    public JGraphTOperations(Graph<FlowNode, TypedGraphMLEdge> graph) {
        this.graph = graph;
    }

    public boolean addNode(FlowNode node) {
        return graph.addVertex(node);
    }

    public boolean connect(FlowNode from, FlowNode to, String edgeType) {
        return graph.addEdge(from, to, new TypedGraphMLEdge(edgeType));
    }
}
