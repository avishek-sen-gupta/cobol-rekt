package org.smojol.common.vm.interpreter;

import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;

import java.util.ArrayList;
import java.util.List;

public class ExecutionListeners implements ExecutionListener {
    List<ExecutionListener> listeners;

    public ExecutionListeners(List<ExecutionListener> listeners) {
        this.listeners = new ArrayList<>(listeners);
    }

    @Override
    public void visit(String message, FlowNode node, FlowNodeService nodeService) {
        listeners.forEach(l -> l.visit(message, node, nodeService));
    }

    @Override
    public void notify(String message, FlowNode node, FlowNodeService nodeService) {
        listeners.forEach(l -> l.notify(message, node, nodeService));
    }

    @Override
    public void notifyTermination() {
        listeners.forEach(ExecutionListener::notifyTermination);
    }
}
