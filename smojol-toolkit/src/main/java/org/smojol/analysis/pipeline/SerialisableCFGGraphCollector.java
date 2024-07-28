package org.smojol.analysis.pipeline;


import com.mojo.woof.NodeRelations;
import lombok.Getter;
import org.smojol.common.ast.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class SerialisableCFGGraphCollector implements FlowNodeVisitor {
    private final List<SerialisableCFGFlowNode> allNodes = new ArrayList<>();
    private final List<SerialisableCFGFEdge> allEdges = new ArrayList<>();

    @Override
    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext context, FlowNodeService nodeService) {
        allNodes.add(new SerialisableCFGFlowNode(node));
        allEdges.addAll(outgoingNodes.stream()
                .map(o -> new SerialisableCFGFEdge(UUID.randomUUID().toString(), node.id(), o.id(), NodeRelations.FOLLOWED_BY)).toList());
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService) {
        visitParentChildLink(parent, internalTreeRoot, ctx, nodeService, FlowNodeCondition.ALWAYS_SHOW);
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
        allEdges.add(new SerialisableCFGFEdge(UUID.randomUUID().toString(), parent.id(),
                internalTreeRoot.id(), NodeRelations.STARTS_WITH));
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {
        allEdges.add(new SerialisableCFGFEdge(UUID.randomUUID().toString(), from.id(),
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
