package org.smojol.toolkit.analysis.graph.graphml;

import lombok.Getter;
import org.jgrapht.Graph;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.jgrapht.JGraphTDataOperations;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;

import static com.mojo.woof.NodeRelations.CONTAINS_DATA;

public class GraphMLDataStructureVisitor implements DataStructureVisitor {
    @Getter private final Graph<TypedGraphVertex, TypedGraphEdge> dataStructuresGraph;
    private final NodeSpecBuilder nodeQualifier;
    private final JGraphTDataOperations graphOperations;

    public GraphMLDataStructureVisitor(Graph<TypedGraphVertex, TypedGraphEdge> dataStructuresGraph, NodeSpecBuilder qualifier) {
        this.nodeQualifier = qualifier;
        this.dataStructuresGraph = dataStructuresGraph;
        graphOperations = new JGraphTDataOperations(this.dataStructuresGraph, qualifier);
    }

    @Override
    public CobolDataStructure visit(CobolDataStructure data, CobolDataStructure parent, CobolDataStructure root) {
        graphOperations.addNode(data);
        if (parent == null) return data;
        graphOperations.connect(parent, data, CONTAINS_DATA);
        return data;
    }
}
