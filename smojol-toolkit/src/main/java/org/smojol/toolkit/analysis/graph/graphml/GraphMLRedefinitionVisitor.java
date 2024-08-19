package org.smojol.toolkit.analysis.graph.graphml;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.jgrapht.Graph;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.jgrapht.JGraphTDataOperations;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.graph.DataRedefinitionComputer;

import java.util.Map;

import static com.mojo.woof.NodeRelations.REDEFINES;

public class GraphMLRedefinitionVisitor implements DataStructureVisitor {
    private final Graph<TypedGraphVertex, TypedGraphEdge> graph;
    private final NodeSpecBuilder nodeQualifier;
    private final JGraphTDataOperations graphOperations;

    public GraphMLRedefinitionVisitor(Graph<TypedGraphVertex, TypedGraphEdge> graph, NodeSpecBuilder qualifier) {
        this.graph = graph;
        this.nodeQualifier = qualifier;
        graphOperations = new JGraphTDataOperations(this.graph, qualifier);
    }

    @Override
    public CobolDataStructure visit(CobolDataStructure data, CobolDataStructure parent, CobolDataStructure root) {
        Map.Entry<CobolDataStructure, CobolDataStructure> redefinitionPair = DataRedefinitionComputer.redefinitionPair(data, root);
        if (ImmutablePair.nullPair().equals(redefinitionPair)) return data;
        graphOperations.connect(redefinitionPair.getKey(), redefinitionPair.getValue(), REDEFINES);
        return data;
    }

}

