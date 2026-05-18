package org.smojol.toolkit.ast;

import java.util.List;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.ast.NullFlowNode;

public class FlowNodes {
  private final List<FlowNode> nodes;
  private final FlowNodeService nodeService;

  public FlowNodes(List<FlowNode> nodes, FlowNodeService nodeService) {
    this.nodes = nodes;
    this.nodeService = nodeService;
  }

  public FlowNode first() {
    if (nodes.isEmpty()) return new NullFlowNode(nodeService);
    return nodes.getFirst();
  }
}
