package org.smojol.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.common.flowchart.FlowNodeType;
import org.smojol.common.vm.stack.StackFrames;

public class ParagraphNameFlowNode extends CobolFlowNode {
    public ParagraphNameFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.PARAGRAPH_NAME;
    }

    @Override
    public FlowNode passthrough() {
        System.out.println(String.format("%s has %s outgoing nodes", executionContext.getText(), outgoingNodes.size()));
        // In case of an empty paragraph, there is nothing to terminate with, so we return this
        // TODO: But check why IDMS copy book MAP-BINDS is missing
        if (outgoingNodes.isEmpty()) return this;
        return outgoingNodes.getFirst();
//        return this;
    }

    @Override
    public boolean isPassthrough() {
        return true;
    }
}
