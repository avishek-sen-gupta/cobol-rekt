package org.smojol.toolkit.analysis.pipeline;


import com.mojo.woof.NodeRelations;
import org.smojol.common.ast.*;
import org.smojol.common.id.IdProvider;

import java.util.ArrayList;
import java.util.List;

public class SerialisableCFGGraphCollector implements FlowNodeVisitor {
    private final List<SerialisableCFGFlowNode> nodes = new ArrayList<>();
    private final List<SerialisableEdge> edges = new ArrayList<>();
    private final IdProvider idProvider;

    public SerialisableCFGGraphCollector(IdProvider idProvider) {
        this.idProvider = idProvider;
    }

    @Override
    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext context, FlowNodeService nodeService) {
        nodes.add(new SerialisableCFGFlowNode(node));
        edges.addAll(outgoingNodes.stream()
                .map(o -> new SerialisableEdge(idProvider.next(), node.id(), o.id(), NodeRelations.FOLLOWED_BY)).toList());
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService) {
        visitParentChildLink(parent, internalTreeRoot, ctx, nodeService, FlowNodeCondition.ALWAYS_SHOW);
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
        edges.add(new SerialisableEdge(idProvider.next(), parent.id(),
                internalTreeRoot.id(), NodeRelations.STARTS_WITH));
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {
        edges.add(new SerialisableEdge(idProvider.next(), from.id(),
                to.id(), NodeRelations.JUMPS_TO));
    }

    @Override
    public FlowNodeVisitor newScope(FlowNode enclosingScope) {
        return this;
    }

    @Override
    public void group(FlowNode root) {

    }
}
