package org.smojol.analysis.graph;

import lombok.Getter;
import org.jgrapht.Graph;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;

import static com.mojo.woof.NodeRelations.CONTAINS;

public class GraphMLDataStructureVisitor implements DataStructureVisitor {
    @Getter private final Graph<CobolDataStructure, TypedGraphMLEdge> dataStructuresGraph;
    private final NodeSpecBuilder nodeQualifier;

    public GraphMLDataStructureVisitor(Graph<CobolDataStructure, TypedGraphMLEdge> dataStructuresGraph, NodeSpecBuilder nodeQualifier) {
        this.nodeQualifier = nodeQualifier;
        this.dataStructuresGraph = dataStructuresGraph;
    }

    @Override
    public CobolDataStructure visit(CobolDataStructure data, CobolDataStructure parent, CobolDataStructure root) {
        dataStructuresGraph.addVertex(data);
        if (parent == null) return data;
        dataStructuresGraph.addEdge(parent, data, new TypedGraphMLEdge(CONTAINS));
        return data;
    }
}
