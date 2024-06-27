package org.smojol.common.vm.stack;

import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;

public record ExecutionContext(FlowNode node, StackFrames runtimeStackFrames, FlowNodeService nodeService) {
}
