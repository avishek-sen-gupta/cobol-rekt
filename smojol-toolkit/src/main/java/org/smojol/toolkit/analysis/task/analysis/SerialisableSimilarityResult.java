package org.smojol.toolkit.analysis.task.analysis;

import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.alg.similarity.ZhangShashaTreeEditDistance;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphVertex;
import org.smojol.common.ast.FlowNode;
import org.smojol.toolkit.analysis.pipeline.SerialisableNodeOperation;

import java.util.List;

public class SerialisableSimilarityResult {
    private final String left;
    private final String right;
    private final double distance;
    private final List<SerialisableNodeOperation> operations;

    public SerialisableSimilarityResult(Pair<FlowNode, FlowNode> nodes, double distance,
                                        List<ZhangShashaTreeEditDistance.EditOperation<TypedGraphVertex>> editOperationLists) {
        left = nodes.getLeft().name();
        right = nodes.getRight().name();
        this.distance = distance;
        operations = editOperationLists.stream().map(SerialisableNodeOperation::new).toList();
    }
}
