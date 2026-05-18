package org.smojol.common.vm.interpreter;

import java.util.ArrayList;
import java.util.List;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;

public class ExecutionListeners implements ExecutionListener {
  List<ExecutionListener> listeners;

  public ExecutionListeners(List<ExecutionListener> listeners) {
    this.listeners = new ArrayList<>(listeners);
  }

  @Override
  public void notify(String message, FlowNode node, FlowNodeService nodeService) {
    listeners.forEach(l -> l.notify(message, node, nodeService));
  }

  @Override
  public void visit(FlowNode node, FlowNodeService nodeService) {
    listeners.forEach(l -> l.visit(node, nodeService));
  }

  @Override
  public void visitTermination() {
    listeners.forEach(ExecutionListener::visitTermination);
  }

  @Override
  public void notifyTermination() {
    listeners.forEach(ExecutionListener::notifyTermination);
  }
}
