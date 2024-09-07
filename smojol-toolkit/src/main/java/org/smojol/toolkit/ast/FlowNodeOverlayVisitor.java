package org.smojol.toolkit.ast;

import org.smojol.common.ast.*;
import org.smojol.common.flowchart.*;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

public class FlowNodeOverlayVisitor implements FlowNodeVisitor {
    private static final Logger logger = Logger.getLogger(FlowNodeOverlayVisitor.class.getName());
    private final FlowNode enclosingScope;
    private GenericProcessingFlowNode head;
    private List<GenericProcessingFlowNode> groups;

    public FlowNodeOverlayVisitor(FlowNode enclosingScope) {
        this(enclosingScope, new ArrayList<>());
    }

    public FlowNodeOverlayVisitor(FlowNode enclosingScope, List<GenericProcessingFlowNode> groups) {
        this.enclosingScope = enclosingScope;
        this.groups = groups;
    }

    // TODO: This needs to become idempotent. There will be duplicate groups if the mergeable items do not come from higher up the syntax tree first.
    // TODO: The condition needs to be refactored, probably do not need to be so picky about which types, and just check for isMergeable(), need to test
    // Currently, this works because super.buildInternalFlow() is called before internalTreeRoot.buildFlow() in CompositeCobolNode.
    @Override
    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext context, FlowNodeService nodeService) {
        if ((node.getClass() == SentenceFlowNode.class ||
                node.getClass() == ConditionalStatementFlowNode.class ||
                (MergingChartOverlay.isAtomic(node) && !contained(node)) // This condition is a little sus because technically, statements inside sentences could also get their own groups which would show up in addition to their parent sentence groups. It's working now, need to investigate with a small test program.
        ) && node.isMergeable()) {
            logger.finer("MERGEABLE : " + node);
            if (head == null) {
                head = new GenericProcessingFlowNode(node, enclosingScope, nodeService);
                groups.add(head);
            } else {
                head.add(node);
            }
        } else {
            if (head != null) {
                head = null;
            }
        }
    }

    private boolean contained(FlowNode node) {
        return groups.stream().anyMatch(g -> g.contains(node));
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
        return new FlowNodeOverlayVisitor(enclosingScope, groups);
    }

    @Override
    public void group(FlowNode root) {

    }

    public void report() {
        groups.forEach(g -> logger.finer(g.toString()));
    }

    public ChartOverlay overlay() {
        return new MergingChartOverlay(groups);
    }
}
