package org.smojol.common.graph;

import org.smojol.common.transpiler.TranspilerFlowgraph;

public class RemoveGotoTask {

    private final TranspilerFlowgraph flowgraph;

    public RemoveGotoTask(TranspilerFlowgraph flowgraph) {
        this.flowgraph = flowgraph;
    }

    public void run() {
    }
}
