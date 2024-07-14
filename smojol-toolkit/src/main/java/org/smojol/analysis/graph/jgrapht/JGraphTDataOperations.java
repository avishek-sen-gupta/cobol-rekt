package org.smojol.analysis.graph.jgrapht;

import org.jgrapht.Graph;
import org.smojol.analysis.graph.graphml.TypedGraphMLEdge;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;

public class JGraphTDataOperations {
    private final Graph<CobolDataStructure, TypedGraphMLEdge> graph;

    public JGraphTDataOperations(Graph<CobolDataStructure, TypedGraphMLEdge> graph) {
        this.graph = graph;
    }

    public boolean addNode(CobolDataStructure node) {
        return graph.addVertex(node);
    }

    public boolean connect(CobolDataStructure from, CobolDataStructure to, String edgeType) {
        return graph.addEdge(from, to, new TypedGraphMLEdge(edgeType));
    }

    public boolean containsVertex(CobolDataStructure structure) {
        return graph.containsVertex(structure);
    }
}
