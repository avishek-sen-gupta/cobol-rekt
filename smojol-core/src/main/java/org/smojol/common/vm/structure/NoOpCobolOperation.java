package org.smojol.common.vm.structure;

import org.smojol.common.flowchart.FlowNode;

public class NoOpCobolOperation implements CobolOperation {
    @Override
    public void run(CobolDataStructure cobolDataStructure) {

    }

    public static CobolOperation build(FlowNode node) {
        return new NoOpCobolOperation();
    }
}
