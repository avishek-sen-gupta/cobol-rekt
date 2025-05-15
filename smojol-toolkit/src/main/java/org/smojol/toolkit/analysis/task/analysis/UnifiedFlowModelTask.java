package org.smojol.toolkit.analysis.task.analysis;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.intermediate.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.graphml.JGraphTGraphBuilder;
import org.smojol.toolkit.analysis.graph.graphml.SerialisableUnifiedModel;

public class UnifiedFlowModelTask {
    private final FlowNode flowRoot;
    private final CobolDataStructure dataStructures;
    private final NodeSpecBuilder qualifier;

    public UnifiedFlowModelTask(FlowNode flowRoot, CobolDataStructure dataStructures, NodeSpecBuilder qualifier) {
        this.flowRoot = flowRoot;
        this.dataStructures = dataStructures;
        this.qualifier = qualifier;
    }

    public SerialisableUnifiedModel run() {
        JGraphTGraphBuilder graphMLExporter = new JGraphTGraphBuilder(dataStructures, flowRoot, qualifier);
        graphMLExporter.buildAST();
        graphMLExporter.buildCFG();
        graphMLExporter.buildDataStructures();
        SerialisableUnifiedModel unifiedModel = graphMLExporter.asSerialisable();
        return unifiedModel;
    }
}
