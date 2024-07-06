package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.WoofNode;
import org.neo4j.driver.Record;
import org.smojol.common.flowchart.*;
import org.smojol.interpreter.navigation.FlowNodeASTTraversal;

import java.util.Map;
import java.util.UUID;

import static com.mojo.woof.NodeLabels.AST_NODE;
import static com.mojo.woof.NodeProperties.*;
import static com.mojo.woof.NodeRelations.CONTAINS;

public class Neo4JASTBuilder {
    private final GraphSDK sdk;

    public Neo4JASTBuilder(GraphSDK sdk) {
        this.sdk = sdk;
    }

    public void build(FlowNode node) {
        new FlowNodeASTTraversal<Record>().build(node, this::make);
    }

    public Record make(FlowNode tree, Record parent) {
        WoofNode node = new WoofNode(Map.of(FLOW_ID, UUID.randomUUID().toString(),
                TEXT, NodeText.originalText(tree.getExecutionContext(), NodeText::PASSTHROUGH),
                TYPE, tree.type().toString()),
                ImmutableList.of(AST_NODE, tree.type().toString()));
        Record record = sdk.createNode(node);
        if (parent == null) return record;
        sdk.connect(parent, record, CONTAINS);
        return record;
    }

    private Boolean stopAtSentence(FlowNode tree) {
        return tree.type() == FlowNodeType.SECTION;
    }
}
