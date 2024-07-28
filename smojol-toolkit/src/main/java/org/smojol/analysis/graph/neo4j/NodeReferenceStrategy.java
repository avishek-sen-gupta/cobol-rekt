package org.smojol.analysis.graph.neo4j;

import com.mojo.woof.GraphSDK;
import org.neo4j.driver.Record;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.common.ast.FlowNode;

import static com.mojo.woof.NodeLabels.AST_NODE;
import static com.mojo.woof.NodeLabels.CFG_NODE;

public interface NodeReferenceStrategy {
    NodeReferenceStrategy NEW_AST_NODE = new NewNodeReferenceStrategy(AST_NODE);
    NodeReferenceStrategy EXISTING_CFG_NODE = new ExistingNodeReferenceStrategy(CFG_NODE);
    NodeReferenceStrategy EXISTING_AST_NODE = new ExistingNodeReferenceStrategy(AST_NODE);

    Record reference(FlowNode tree, GraphSDK sdk, NodeSpecBuilder qualifier);
}
