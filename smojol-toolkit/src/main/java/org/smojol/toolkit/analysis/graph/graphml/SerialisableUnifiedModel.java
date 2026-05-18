package org.smojol.toolkit.analysis.graph.graphml;

import java.util.List;
import org.smojol.common.ast.SerialisableCFGFlowNode;
import org.smojol.common.ast.SerialisableEdge;
import org.smojol.toolkit.analysis.pipeline.SerialisableCobolDataStructure;

public record SerialisableUnifiedModel(
    List<SerialisableCFGFlowNode> codeVertices,
    List<SerialisableCobolDataStructure> dataVertices,
    List<SerialisableEdge> edges) {}
