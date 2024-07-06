package org.smojol.interpreter.interpreter;

import com.mojo.woof.GraphSDK;
import com.mojo.woof.NodeRelations;
import com.mojo.woof.WoofNode;
import org.neo4j.driver.Record;
import org.smojol.analysis.graph.FlowToWoof;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.common.flowchart.FlowNodeType;
import org.smojol.common.vm.interpreter.ExecutionListener;

import java.util.ArrayList;
import java.util.List;

public class Neo4JExecutionTracer implements ExecutionListener {
    private final GraphSDK sdk;
    private List<FlowNode> path = new ArrayList<>();

    public Neo4JExecutionTracer(GraphSDK sdk) {
        this.sdk = sdk;
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
    public void notifyTermination() {
        Record current = sdk.createNode(FlowToWoof.toWoofNode(path.getFirst()));
        for (int i = 0; i <= path.size() - 2; i++) {
            Record next = sdk.createNode(FlowToWoof.toWoofNode(path.get(i + 1)));
            sdk.connect(current, next, NodeRelations.FOLLOWED_BY);
            current = next;
        }
    }
}
