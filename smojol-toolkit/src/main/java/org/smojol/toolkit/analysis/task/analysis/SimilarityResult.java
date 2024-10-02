package org.smojol.toolkit.analysis.task.analysis;

import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.alg.similarity.ZhangShashaTreeEditDistance;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphVertex;
import org.smojol.common.ast.FlowNode;

import java.util.List;

public record SimilarityResult(Pair<FlowNode, FlowNode> nodes, double distance,
                               List<ZhangShashaTreeEditDistance.EditOperation<TypedGraphVertex>> editOperationLists) {
}
