package org.smojol.common.vm.interpreter;

import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;

public interface ExecutionListener {
    void notify(String message, FlowNode node, FlowNodeService nodeService);
    void visit(FlowNode node, FlowNodeService nodeService);
    void visitTermination();
    void notifyTermination();
}
