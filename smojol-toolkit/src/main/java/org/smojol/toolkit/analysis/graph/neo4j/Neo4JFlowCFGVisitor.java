package org.smojol.toolkit.analysis.graph.neo4j;

import com.mojo.woof.GraphSDK;
import org.neo4j.driver.Record;
import org.smojol.toolkit.intermediate.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.NodeToWoof;
import org.smojol.common.ast.*;

import java.util.List;

public class Neo4JFlowCFGVisitor implements FlowNodeVisitor {
    private final GraphSDK sdk;
    private final NodeSpecBuilder qualifier;

    public Neo4JFlowCFGVisitor(GraphSDK sdk, NodeSpecBuilder qualifier) {
        this.sdk = sdk;
        this.qualifier = qualifier;
    }

    @Override
    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext context, FlowNodeService nodeService) {
        Record sourceRecord = newOrExisting(node);
        node.getCommentBlocks().forEach(cb -> sdk.hasComment(sourceRecord, sdk.comment(NodeToWoof.toWoofNode(cb, qualifier))));
        outgoingNodes.forEach(o -> {
            Record destinationRecord = newOrExisting(o);
            sdk.isFollowedBy(sourceRecord, destinationRecord);
        });
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService) {
        Record parentRecord = newOrExisting(parent);
        Record childRecord = newOrExisting(internalTreeRoot);

        sdk.startsWith(parentRecord, childRecord);
    }

    private Record newOrExisting(FlowNode node) {
        return NodeToWoof.newOrExistingCFGNode(node, sdk, qualifier);
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
        visitParentChildLink(parent, internalTreeRoot, ctx, nodeService);
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {
        Record sourceRecord = newOrExisting(from);
        Record destinationRecord = newOrExisting(to);
        sdk.jumpsTo(sourceRecord, destinationRecord);
    }

    @Override
    public FlowNodeVisitor newScope(FlowNode enclosingScope) {
        return this;
    }

    @Override
    public void group(FlowNode root) {

    }
}
