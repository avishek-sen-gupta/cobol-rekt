package org.smojol.analysis.graph.graphml;

import lombok.Getter;
import org.jgrapht.Graph;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.analysis.graph.jgrapht.JGraphTDataOperations;
import org.smojol.analysis.graph.jgrapht.JGraphTOperations;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;

import static com.mojo.woof.NodeRelations.CONTAINS;

public class GraphMLDataStructureVisitor implements DataStructureVisitor {
    @Getter private final Graph<CobolDataStructure, TypedGraphMLEdge> dataStructuresGraph;
    private final NodeSpecBuilder nodeQualifier;
    private final JGraphTDataOperations graphOperations;

    public GraphMLDataStructureVisitor(Graph<CobolDataStructure, TypedGraphMLEdge> dataStructuresGraph, NodeSpecBuilder nodeQualifier) {
        this.nodeQualifier = nodeQualifier;
        this.dataStructuresGraph = dataStructuresGraph;
        graphOperations = new JGraphTDataOperations(this.dataStructuresGraph);
    }

    @Override
    public CobolDataStructure visit(CobolDataStructure data, CobolDataStructure parent, CobolDataStructure root) {
        graphOperations.addNode(data);
        if (parent == null) return data;
        graphOperations.connect(parent, data, CONTAINS);
        return data;
    }
}
