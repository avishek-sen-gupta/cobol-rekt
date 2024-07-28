package org.smojol.common.ast;


import java.util.List;

public interface FlowNodeVisitor {
    void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext context, FlowNodeService nodeService);
    void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService);
    void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService, FlowNodeCondition hideStrategy);
    void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext);
    FlowNodeVisitor newScope(FlowNode enclosingScope);
    void group(FlowNode root);
}
