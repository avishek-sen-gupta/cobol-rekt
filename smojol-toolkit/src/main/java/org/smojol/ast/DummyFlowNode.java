package org.smojol.ast;

import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.common.flowchart.FlowNodeType;
import org.smojol.common.flowchart.FlowNodeVisitor;
import org.smojol.common.vm.stack.StackFrames;

public class DummyFlowNode extends CobolFlowNode {
    public DummyFlowNode(FlowNodeService nodeService, StackFrames stackFrames) {
        super(null, null, nodeService, stackFrames);
    }

    @Override
    public void buildFlow() {
        super.buildFlow();
    }

    @Override
    public String toString() {
        return "DUMMY_NODE";
    }

    @Override
    public void acceptUnvisited(FlowNodeVisitor visitor, int level) {
//        super.acceptUnvisited(visitor, level);
    }

    @Override
    public String name() {
        return "DUMMY_EXECUTION_CONTEXT";
    }

    @Override
    public boolean equals(Object o) {
        return o == this;
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.DUMMY;
    }
}
