package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.WoofNode;
import org.neo4j.driver.Record;
import org.smojol.common.flowchart.*;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.Map;
import java.util.UUID;

public class Neo4JDataStructureVisitor implements DataStructureVisitor {
    private final GraphSDK sdk;

    public Neo4JDataStructureVisitor(GraphSDK sdk) {
        this.sdk = sdk;
    }

    @Override
    public CobolDataStructure visit(CobolDataStructure data, CobolDataStructure parent) {
        WoofNode node = new WoofNode(Map.of("dataStructureID", UUID.randomUUID().toString(),
                "name", data.name(),
                "type", data.getDataType().toString(),
                "level", data.getLevelNumber()
                ),
                ImmutableList.of("DATA_STRUCTURE", data.getDataType().toString()));

        Record record = sdk.createNode(node);
        if (parent == null) return data;
        Record parentNode = sdk.findNode(ImmutableList.of("DATA_STRUCTURE"), Map.of("name", parent.name())).getFirst();
        sdk.connect(parentNode, record, "CONTAINS");
        return data;
    }
}
