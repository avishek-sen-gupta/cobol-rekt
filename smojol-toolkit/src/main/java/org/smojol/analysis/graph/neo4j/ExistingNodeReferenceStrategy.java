package org.smojol.analysis.graph.neo4j;

import com.mojo.woof.GraphSDK;
import org.neo4j.driver.Record;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.common.flowchart.FlowNode;

public class ExistingNodeReferenceStrategy implements NodeReferenceStrategy {
    private final String nodeType;

    public ExistingNodeReferenceStrategy(String nodeType) {
        this.nodeType = nodeType;
    }

    @Override
    public Record reference(FlowNode tree, GraphSDK sdk, NodeSpecBuilder qualifier) {
        return sdk.findNodes(qualifier.labelledNodeSearchSpec(tree, nodeType)).getFirst();
    }

}
