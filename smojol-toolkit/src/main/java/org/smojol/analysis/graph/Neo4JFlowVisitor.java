package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.WoofNode;
import org.neo4j.driver.Record;
import org.smojol.common.flowchart.*;

import java.util.List;
import java.util.Map;

public class Neo4JFlowVisitor implements FlowNodeVisitor {
    private final GraphSDK sdk;

    public Neo4JFlowVisitor(GraphSDK sdk) {
        this.sdk = sdk;
    }

    @Override
    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext context, FlowNodeService nodeService) {
        outgoingNodes.forEach(o -> {
            Record sourceRecord = newOrExisting(node);
            Record destinationRecord = newOrExisting(o);
            sdk.connect(sourceRecord, destinationRecord, "FOLLOWED_BY");
        });
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService) {
        Record parentRecord = newOrExisting(parent);
        Record childRecord = newOrExisting(internalTreeRoot);

        sdk.connect(parentRecord, childRecord, "STARTS_WITH");
    }

    private Record newOrExisting(FlowNode node) {
        return sdk.newOrExisting(ImmutableList.of(), Map.of("flowID", node.id()), new WoofNode(Map.of("flowID", node.id(),
                "text", node.getExecutionContext().getText(),
                "type", node.type().toString()),
                ImmutableList.of("CFG_NODE", node.type().toString())));
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
        visitParentChildLink(parent, internalTreeRoot, ctx, nodeService);
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {
        Record sourceRecord = newOrExisting(from);
        Record destinationRecord = newOrExisting(to);
        sdk.connect(sourceRecord, destinationRecord, "JUMPS_TO");
    }
    @Override
    public FlowNodeVisitor newScope(FlowNode enclosingScope) {
        return this;
    }

    @Override
    public void group(FlowNode root) {

    }
}
