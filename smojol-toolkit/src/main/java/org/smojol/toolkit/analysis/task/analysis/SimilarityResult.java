package org.smojol.toolkit.analysis.task.analysis;

import com.mojo.algorithms.domain.TypedGraphVertex;
import java.util.List;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.alg.similarity.ZhangShashaTreeEditDistance;
import org.smojol.common.ast.FlowNode;

public record SimilarityResult(
    Pair<FlowNode, FlowNode> nodes,
    double distance,
    List<ZhangShashaTreeEditDistance.EditOperation<TypedGraphVertex>> editOperationLists) {}
