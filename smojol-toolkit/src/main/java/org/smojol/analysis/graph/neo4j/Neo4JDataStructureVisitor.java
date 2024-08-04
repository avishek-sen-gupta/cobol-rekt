package org.smojol.analysis.graph.neo4j;

import com.mojo.woof.*;
import org.neo4j.driver.Record;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.analysis.graph.NodeToWoof;
import org.smojol.common.ast.CommentBlock;
import org.smojol.common.flowchart.*;
import org.smojol.common.vm.structure.CobolDataStructure;

import static org.smojol.analysis.graph.NodeToWoof.dataStructureToWoof;

public class Neo4JDataStructureVisitor implements DataStructureVisitor {
    private final GraphSDK sdk;
    private final NodeSpecBuilder nodeQualifier;

    public Neo4JDataStructureVisitor(GraphSDK sdk, NodeSpecBuilder nodeQualifier) {
        this.sdk = sdk;
        this.nodeQualifier = nodeQualifier;
    }

    @Override
    public CobolDataStructure visit(CobolDataStructure data, CobolDataStructure parent, CobolDataStructure root) {
        WoofNode dataNode = dataStructureToWoof(data, nodeQualifier);
        Record dataRecord = sdk.createNode(dataNode);
        for (CommentBlock cb: data.getCommentBlocks()) {
            WoofNode commentNode = NodeToWoof.toWoofNode(cb, nodeQualifier);
            sdk.hasComment(dataRecord, sdk.comment(commentNode));
        }
        if (parent == null) return data;
        Record parentNode = sdk.findNodes(nodeQualifier.dataNodeSearchSpec(parent)).getFirst();
        sdk.containsDataNode(parentNode, dataRecord);
        return data;
    }
}
