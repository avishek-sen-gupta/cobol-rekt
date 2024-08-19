package org.smojol.toolkit.analysis.graph.graphml;

import org.smojol.toolkit.analysis.pipeline.SerialisableCobolDataStructure;
import org.smojol.common.ast.SerialisableCFGFlowNode;
import org.smojol.common.ast.SerialisableEdge;

import java.util.List;

public record SerialisableUnifiedModel(List<SerialisableCFGFlowNode> codeVertices,
                                       List<SerialisableCobolDataStructure> dataVertices,
                                       List<SerialisableEdge> edges) {
}
