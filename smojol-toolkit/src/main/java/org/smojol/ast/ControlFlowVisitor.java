package org.smojol.ast;

import org.smojol.common.flowchart.*;

import java.util.List;

public class ControlFlowVisitor implements FlowNodeVisitor {
    @Override
    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext context, FlowNodeService nodeService) {
        node.buildControlFlow();
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService) {
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {

    }

    @Override
    public FlowNodeVisitor newScope(FlowNode enclosingScope) {
        return new ControlFlowVisitor();
    }

    @Override
    public void group(FlowNode root) {

    }

}
