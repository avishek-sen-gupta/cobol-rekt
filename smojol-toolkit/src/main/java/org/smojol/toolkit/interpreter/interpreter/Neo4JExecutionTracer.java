package org.smojol.toolkit.interpreter.interpreter;

import com.mojo.woof.GraphSDK;
import com.mojo.woof.WoofNode;
import org.neo4j.driver.Record;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.vm.interpreter.ExecutionListener;

import java.util.ArrayList;
import java.util.List;

public class Neo4JExecutionTracer implements ExecutionListener {
    private final GraphSDK sdk;
    private final NodeSpecBuilder qualifier;
    private final List<FlowNode> path = new ArrayList<>();

    public Neo4JExecutionTracer(GraphSDK sdk, NodeSpecBuilder qualifier) {
        this.sdk = sdk;
        this.qualifier = qualifier;
    }

    @Override
    public void notify(String message, FlowNode node, FlowNodeService nodeService) {
    }

    @Override
    public void visit(FlowNode node, FlowNodeService nodeService) {
        if (node.type() == FlowNodeType.SECTION_HEADER
        ||  node.type() == FlowNodeType.PARAGRAPH_NAME
        ||  node.type() == FlowNodeType.EXIT) return;
        path.add(node);
    }

    @Override
    public void visitTermination() {
        Record current = sdk.createNode(new WoofNode(qualifier.newTraceNode(path.getFirst())));
//        Record current = sdk.createNode(NodeToWoof.toWoofTraceNode(path.getFirst(), qualifier));
        for (int i = 0; i <= path.size() - 2; i++) {
//            Record next = sdk.createNode(NodeToWoof.toWoofTraceNode(path.get(i + 1), qualifier));
            Record next = sdk.createNode(new WoofNode(qualifier.newTraceNode(path.get(i + 1))));
            sdk.isFollowedBy(current, next);
            current = next;
        }
    }

    @Override
    public void notifyTermination() {

    }
}
