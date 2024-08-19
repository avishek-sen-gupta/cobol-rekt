package org.smojol.toolkit.analysis.graph.neo4j;

import com.mojo.woof.GraphSDK;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.neo4j.driver.Record;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.graph.DataRedefinitionComputer;

import java.util.Map;

public class Neo4JRedefinitionVisitor implements DataStructureVisitor {
    private final GraphSDK sdk;
    private final NodeSpecBuilder nodeQualifier;

    public Neo4JRedefinitionVisitor(GraphSDK sdk, NodeSpecBuilder nodeQualifier) {
        this.sdk = sdk;
        this.nodeQualifier = nodeQualifier;
    }

    @Override
    public CobolDataStructure visit(CobolDataStructure data, CobolDataStructure parent, CobolDataStructure root) {
        Map.Entry<CobolDataStructure, CobolDataStructure> redefinitionPair = DataRedefinitionComputer.redefinitionPair(data, root);
        if (ImmutablePair.nullPair().equals(redefinitionPair)) return data;
        Record redefinition = sdk.findNodes(nodeQualifier.dataNodeSearchSpec(redefinitionPair.getKey())).getFirst();
        Record redefinedRecord = sdk.findNodes(nodeQualifier.dataNodeSearchSpec(redefinitionPair.getValue())).getFirst();
        sdk.redefines(redefinition, redefinedRecord);
        return data;
    }

}

