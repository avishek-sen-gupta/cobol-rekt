package org.smojol.analysis.graph.jgrapht;

import org.jgrapht.Graph;
import org.smojol.analysis.graph.graphml.TypedCodeVertex;
import org.smojol.analysis.graph.graphml.TypedDataStructureVertex;
import org.smojol.analysis.graph.graphml.TypedGraphEdge;
import org.smojol.analysis.graph.graphml.TypedGraphVertex;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;

public class JGraphTDataOperations {
    private final Graph<TypedGraphVertex, TypedGraphEdge> graph;

    public JGraphTDataOperations(Graph<TypedGraphVertex, TypedGraphEdge> graph) {
        this.graph = graph;
    }

    public boolean addNode(CobolDataStructure node) {
        return graph.addVertex(new TypedDataStructureVertex(node));
    }

    public boolean connect(CobolDataStructure from, CobolDataStructure to, String edgeType) {
        TypedGraphVertex vFrom = new TypedDataStructureVertex(from);
        TypedGraphVertex vTo = new TypedDataStructureVertex(to);
        if (!graph.containsVertex(vFrom)) graph.addVertex(vFrom);
        if (!graph.containsVertex(vTo)) graph.addVertex(vTo);
        return graph.addEdge(vFrom, vTo, new TypedGraphEdge(edgeType));
    }

    public boolean containsVertex(CobolDataStructure structure) {
        return graph.containsVertex(new TypedDataStructureVertex(structure));
    }

    public boolean connect(FlowNode from, CobolDataStructure to, String edgeType) {
        TypedGraphVertex vFrom = new TypedCodeVertex(from);
        TypedGraphVertex vTo = new TypedDataStructureVertex(to);
        return graph.addEdge(vFrom, vTo, new TypedGraphEdge(edgeType));
    }
}
