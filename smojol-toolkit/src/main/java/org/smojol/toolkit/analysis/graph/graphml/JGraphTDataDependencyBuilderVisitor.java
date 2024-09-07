package org.smojol.toolkit.analysis.graph.graphml;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeASTVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.graph.DataDependencyPairComputer;
import org.smojol.toolkit.analysis.graph.jgrapht.JGraphTDataOperations;

import java.util.List;

import static com.mojo.woof.NodeRelations.*;
import static com.mojo.woof.NodeRelations.FLOWS_INTO;

public class JGraphTDataDependencyBuilderVisitor extends FlowNodeASTVisitor<CobolDataStructure> {
    private final JGraphTDataOperations dataGraphOperations;
    private final CobolDataStructure dataRoot;

    public JGraphTDataDependencyBuilderVisitor(JGraphTDataOperations jGraphTDataOperations, CobolDataStructure dataRoot) {
        this(jGraphTDataOperations, dataRoot, null);
    }

    public JGraphTDataDependencyBuilderVisitor(JGraphTDataOperations jGraphTDataOperations, CobolDataStructure dataRoot, CobolDataStructure ancestor) {
        super(ancestor);
        this.dataGraphOperations = jGraphTDataOperations;
        this.dataRoot = dataRoot;
    }

    @Override
    public CobolDataStructure visit(FlowNode node) {
        Pair<List<CobolDataStructure>, List<CobolDataStructure>> pairs = DataDependencyPairComputer.dependencyPairs(node, dataRoot);
        if (ImmutablePair.nullPair().equals(pairs)) return dataRoot;
        if (pairs.getValue().isEmpty()) {
            accesses(node, pairs.getKey());
            return dataRoot;
        }
        connect(pairs.getLeft(), pairs.getRight(), node);
        return dataRoot;
    }

    @Override
    public FlowNodeASTVisitor<CobolDataStructure> scope(FlowNode n, CobolDataStructure visitResult) {
        return this;
    }

    private void accesses(FlowNode node, List<CobolDataStructure> accessedStructures) {
        accessedStructures.forEach(s -> {
            if (!dataGraphOperations.containsVertex(s)) dataGraphOperations.addNode(s);
            dataGraphOperations.connect(node, s, ACCESSES);
        });
    }

    private void connect(List<CobolDataStructure> froms, List<CobolDataStructure> tos, FlowNode node) {
        tos.forEach(to -> {
            if (!dataGraphOperations.containsVertex(to)) dataGraphOperations.addNode(to);
            dataGraphOperations.connect(node, to, MODIFIES);
            froms.forEach(from -> {
                if (!dataGraphOperations.containsVertex(from)) dataGraphOperations.addNode(from);
                dataGraphOperations.connect(node, from, ACCESSES);
                dataGraphOperations.connect(from, to, FLOWS_INTO);
            });
        });
    }
}
