package org.smojol.analysis.graph.graphml;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.jgrapht.Graph;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.Map;

import static com.mojo.woof.NodeRelations.REDEFINES;
import static org.smojol.analysis.graph.DataRedefinitionComputer.redefinitionPair;

public class GraphMLRedefinitionVisitor implements DataStructureVisitor {
    private final Graph<CobolDataStructure, TypedGraphMLEdge> graph;
    private final NodeSpecBuilder nodeQualifier;

    public GraphMLRedefinitionVisitor(Graph<CobolDataStructure, TypedGraphMLEdge> graph, NodeSpecBuilder nodeQualifier) {
        this.graph = graph;
        this.nodeQualifier = nodeQualifier;
    }

    @Override
    public CobolDataStructure visit(CobolDataStructure data, CobolDataStructure parent, CobolDataStructure root) {
        Map.Entry<CobolDataStructure, CobolDataStructure> redefinitionPair = redefinitionPair(data, root);
        if (ImmutablePair.nullPair().equals(redefinitionPair)) return data;
        graph.addEdge(redefinitionPair.getKey(), redefinitionPair.getValue(), new TypedGraphMLEdge(REDEFINES));
        return data;
    }

}

