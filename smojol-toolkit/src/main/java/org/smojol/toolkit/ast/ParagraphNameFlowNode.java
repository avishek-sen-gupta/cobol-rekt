package org.smojol.toolkit.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.AggregatingFlowNodeASTVisitor;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.vm.stack.StackFrames;

import java.util.logging.Logger;

public class ParagraphNameFlowNode extends CobolFlowNode {
    private static final Logger LOGGER = Logger.getLogger(ParagraphNameFlowNode.class.getName());
    public ParagraphNameFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.PARAGRAPH_NAME;
    }

    @Override
    public FlowNode passthrough() {
        LOGGER.finer(String.format("%s has %s outgoing nodes", executionContext.getText(), outgoingNodes.size()));
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
