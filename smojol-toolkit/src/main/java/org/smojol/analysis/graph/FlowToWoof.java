package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.WoofNode;
import org.neo4j.driver.Record;
import org.smojol.common.flowchart.FlowNode;

import java.util.Map;

import static com.mojo.woof.NodeLabels.CFG_NODE;
import static com.mojo.woof.NodeProperties.*;

public class FlowToWoof {
    public static Record newOrExisting(FlowNode node, GraphSDK sdk) {
        return sdk.newOrExisting(ImmutableList.of(), Map.of(FLOW_ID, node.id()), toWoofNode(node));
    }

    public static WoofNode toWoofNode(FlowNode node) {
        return new WoofNode(Map.of(FLOW_ID, node.id(),
                TEXT, node.getExecutionContext().getText(),
                TYPE, node.type().toString()),
                ImmutableList.of(CFG_NODE, node.type().toString()));
    }
}
