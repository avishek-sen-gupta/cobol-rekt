package org.smojol.toolkit.analysis.graph.jgrapht;

import com.mojo.algorithms.domain.TypedGraphEdge;
import com.mojo.algorithms.domain.TypedGraphVertex;
import org.jgrapht.Graph;
import org.smojol.toolkit.intermediate.NodeSpecBuilder;
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
//        return sourceGraph.addVertex(new TypedDataStructureVertex(node));
        return graph.addVertex(qualifier.newDataVertex(node));
    }

    public boolean connect(CobolDataStructure from, CobolDataStructure to, String edgeType) {
//        TypedGraphVertex vFrom = new TypedDataStructureVertex(from);
        TypedGraphVertex vFrom = qualifier.newDataVertex(from);
//        TypedGraphVertex vTo = new TypedDataStructureVertex(to);
        TypedGraphVertex vTo = qualifier.newDataVertex(to);
        if (!graph.containsVertex(vFrom)) graph.addVertex(vFrom);
        if (!graph.containsVertex(vTo)) graph.addVertex(vTo);
//        return sourceGraph.addEdge(vFrom, vTo, new TypedGraphEdge(edgeType));
        return graph.addEdge(vFrom, vTo, qualifier.newEdge(edgeType));
    }

    public boolean containsVertex(CobolDataStructure structure) {
//        return sourceGraph.containsVertex(new TypedDataStructureVertex(structure));
        return graph.containsVertex(qualifier.newDataVertex(structure));
    }

    public boolean connect(FlowNode from, CobolDataStructure to, String edgeType) {
//        TypedGraphVertex vFrom = new TypedCodeVertex(from);
        TypedGraphVertex vFrom = qualifier.newCodeVertex(from);
//        TypedGraphVertex vTo = new TypedDataStructureVertex(to);
        TypedGraphVertex vTo = qualifier.newDataVertex(to);
//        return sourceGraph.addEdge(vFrom, vTo, new TypedGraphEdge(edgeType));
        try {
            return graph.addEdge(vFrom, vTo, qualifier.newEdge(edgeType));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
