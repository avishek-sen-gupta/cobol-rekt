package org.smojol.analysis.graph.neo4j;

import com.mojo.woof.GraphSDK;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.neo4j.driver.Record;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.Map;

import static com.mojo.woof.NodeRelations.REDEFINES;
import static org.smojol.analysis.graph.DataRedefinitionComputer.redefinitionPair;

public class Neo4JRedefinitionVisitor implements DataStructureVisitor {
    private final GraphSDK sdk;
    private final NodeSpecBuilder nodeQualifier;

    public Neo4JRedefinitionVisitor(GraphSDK sdk, NodeSpecBuilder nodeQualifier) {
        this.sdk = sdk;
        this.nodeQualifier = nodeQualifier;
    }

    @Override
    public CobolDataStructure visit(CobolDataStructure data, CobolDataStructure parent, CobolDataStructure root) {
        Map.Entry<CobolDataStructure, CobolDataStructure> redefinitionPair = redefinitionPair(data, root);
        if (ImmutablePair.nullPair().equals(redefinitionPair)) return data;
        Record redefinition = sdk.findNodes(nodeQualifier.dataNodeSearchSpec(redefinitionPair.getKey())).getFirst();
        Record redefinedRecord = sdk.findNodes(nodeQualifier.dataNodeSearchSpec(redefinitionPair.getValue())).getFirst();
        sdk.redefines(redefinition, redefinedRecord);
        return data;
    }

}

