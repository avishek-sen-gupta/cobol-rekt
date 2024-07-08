package org.smojol.analysis.graph;

import com.mojo.woof.GraphSDK;
import org.neo4j.driver.Record;
import org.smojol.common.flowchart.*;

import java.util.List;

import static com.mojo.woof.NodeRelations.*;

public class Neo4JFlowCFGVisitor implements FlowNodeVisitor {
    private final GraphSDK sdk;
    private final NodeSpecBuilder qualifier;

    public Neo4JFlowCFGVisitor(GraphSDK sdk, NodeSpecBuilder qualifier) {
        this.sdk = sdk;
        this.qualifier = qualifier;
    }

    @Override
    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext context, FlowNodeService nodeService) {
        outgoingNodes.forEach(o -> {
            Record sourceRecord = newOrExisting(node);
            Record destinationRecord = newOrExisting(o);
            sdk.connect(sourceRecord, destinationRecord, FOLLOWED_BY);
        });
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService) {
        Record parentRecord = newOrExisting(parent);
        Record childRecord = newOrExisting(internalTreeRoot);

        sdk.connect(parentRecord, childRecord, STARTS_WITH);
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
        sdk.connect(sourceRecord, destinationRecord, JUMPS_TO);
    }
    @Override
    public FlowNodeVisitor newScope(FlowNode enclosingScope) {
        return this;
    }

    @Override
    public void group(FlowNode root) {

    }
}
