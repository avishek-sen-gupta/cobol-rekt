package org.smojol.analysis.graph.graphml;

import org.smojol.analysis.pipeline.SerialisableCobolDataStructure;
import org.smojol.common.ast.SerialisableCFGFlowNode;

import java.util.List;

public record SerialisableUnifiedModel(List<SerialisableCFGFlowNode> codeVertices,
                                       List<SerialisableCobolDataStructure> dataVertices,
                                       List<org.smojol.common.ast.SerialisableCFGFEdge> edges) {
}
