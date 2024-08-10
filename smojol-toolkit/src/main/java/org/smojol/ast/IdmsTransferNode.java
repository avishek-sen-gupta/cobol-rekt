package org.smojol.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.*;
import org.smojol.common.vm.stack.StackFrames;

public class IdmsTransferNode extends CobolFlowNode {
    public IdmsTransferNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames staticFrameContext) {
        super(parseTree, scope, nodeService, staticFrameContext);
    }

    @Override
    public String name() {
        return NodeText.idmsOriginalText(getExecutionContext(), nodeService);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.TRANSFER;
    }
}
