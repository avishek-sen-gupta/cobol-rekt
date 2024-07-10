package org.smojol.analysis.graph;

import com.mojo.woof.*;
import org.neo4j.driver.Record;
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
        WoofNode node = dataStructureToWoof(data, nodeQualifier);
        Record record = sdk.createNode(node);
        if (parent == null) return data;
        Record parentNode = sdk.findNodes(nodeQualifier.dataNodeSearchSpec(parent)).getFirst();
        sdk.contains(parentNode, record);
        return data;
    }
}
