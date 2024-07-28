package org.smojol.analysis.graph.neo4j;

import com.mojo.woof.GraphSDK;
import com.mojo.woof.WoofNode;
import org.neo4j.driver.Record;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.common.ast.FlowNode;

public class NewNodeReferenceStrategy implements NodeReferenceStrategy {
    private final String nodeType;

    public NewNodeReferenceStrategy(String nodeType) {
        this.nodeType = nodeType;
    }

    @Override
    public Record reference(FlowNode tree, GraphSDK sdk, NodeSpecBuilder qualifier) {
        WoofNode node = new WoofNode(qualifier.labelledCodeNode(tree, nodeType));
        return sdk.createNode(node);
    }

}
