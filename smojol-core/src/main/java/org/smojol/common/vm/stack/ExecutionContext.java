package org.smojol.common.vm.stack;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;

public record ExecutionContext(FlowNode node, StackFrames runtimeStackFrames, FlowNodeService nodeService) {
}
