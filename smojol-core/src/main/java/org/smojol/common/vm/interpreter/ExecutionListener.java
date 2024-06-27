package org.smojol.common.vm.interpreter;

import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;

public interface ExecutionListener {
    void visit(String message, FlowNode node, FlowNodeService nodeService);
    void notify(String message, FlowNode node, FlowNodeService nodeService);
    void notifyTermination();
}
