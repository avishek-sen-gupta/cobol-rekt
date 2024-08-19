package org.smojol.toolkit.analysis.graph.jgrapht;

import org.jgrapht.Graph;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphEdge;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphVertex;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;

public class JGraphTDataOperations {
    private final Graph<TypedGraphVertex, TypedGraphEdge> graph;
    private final NodeSpecBuilder qualifier;

    public JGraphTDataOperations(Graph<TypedGraphVertex, TypedGraphEdge> graph, NodeSpecBuilder qualifier) {
        this.graph = graph;
        this.qualifier = qualifier;
    }

    public boolean addNode(CobolDataStructure node) {
//        return graph.addVertex(new TypedDataStructureVertex(node));
        return graph.addVertex(qualifier.newDataVertex(node));
    }

    public boolean connect(CobolDataStructure from, CobolDataStructure to, String edgeType) {
//        TypedGraphVertex vFrom = new TypedDataStructureVertex(from);
        TypedGraphVertex vFrom = qualifier.newDataVertex(from);
//        TypedGraphVertex vTo = new TypedDataStructureVertex(to);
        TypedGraphVertex vTo = qualifier.newDataVertex(to);
        if (!graph.containsVertex(vFrom)) graph.addVertex(vFrom);
        if (!graph.containsVertex(vTo)) graph.addVertex(vTo);
//        return graph.addEdge(vFrom, vTo, new TypedGraphEdge(edgeType));
        return graph.addEdge(vFrom, vTo, qualifier.newEdge(edgeType));
    }

    public boolean containsVertex(CobolDataStructure structure) {
//        return graph.containsVertex(new TypedDataStructureVertex(structure));
        return graph.containsVertex(qualifier.newDataVertex(structure));
    }

    public boolean connect(FlowNode from, CobolDataStructure to, String edgeType) {
//        TypedGraphVertex vFrom = new TypedCodeVertex(from);
        TypedGraphVertex vFrom = qualifier.newCodeVertex(from);
//        TypedGraphVertex vTo = new TypedDataStructureVertex(to);
        TypedGraphVertex vTo = qualifier.newDataVertex(to);
//        return graph.addEdge(vFrom, vTo, new TypedGraphEdge(edgeType));
        return graph.addEdge(vFrom, vTo, qualifier.newEdge(edgeType));
    }
}
