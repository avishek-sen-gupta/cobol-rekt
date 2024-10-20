package org.smojol.toolkit.ast;

import guru.nidi.graphviz.model.MutableNode;
import lombok.Getter;
import org.smojol.common.ast.*;
import org.smojol.common.flowchart.*;
import org.smojol.toolkit.flowchart.FlowchartStylePreferences;

import java.util.ArrayList;
import java.util.List;

public class FlowNodeCollectorVisitor implements FlowNodeVisitor {

    @Getter private final List<FlowNode> flowNodes;
    private final ChartOverlay overlay;

    public FlowNodeCollectorVisitor(ChartOverlay overlay) {
        this.overlay = overlay;
        flowNodes = new ArrayList<>();
    }

    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext visitContext, FlowNodeService nodeService) {
        FlowNode block = overlay.block(node.passthrough());
        if (flowNodes.contains(block)) return;
        flowNodes.add(block);
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext visitContext, FlowNodeService nodeService) {
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext visitContext, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {
    }

    @Override
    public FlowNodeVisitor newScope(FlowNode enclosingScope) {
        return this;
    }

    @Override
    public void group(FlowNode root) {

    }

    private MutableNode styled(FlowNode flowNode, MutableNode node) {
        return FlowchartStylePreferences.scheme(flowNode).apply(node);
    }
}
