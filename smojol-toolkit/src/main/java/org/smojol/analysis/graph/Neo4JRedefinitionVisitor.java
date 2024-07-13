package org.smojol.analysis.graph;

import com.mojo.woof.GraphSDK;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.neo4j.driver.Record;
import org.smojol.common.flowchart.DataStructureVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.Format1DataStructure;

import java.util.Map;

import static com.mojo.woof.NodeProperties.NAME;
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
//        if (!(data instanceof Format1DataStructure)) return data;
//        CobolParser.DataDescriptionEntryFormat1Context dataDescription = ((Format1DataStructure) data).getDataDescription();
//        if (dataDescription == null || dataDescription.dataRedefinesClause().isEmpty()) return data;
//        String redefinedRecordName = dataDescription.dataRedefinesClause().getFirst().dataName().getText();
//        CobolDataStructure redefinedStruct = root.reference(redefinedRecordName);
        Map.Entry<CobolDataStructure, CobolDataStructure> redefinitionPair = redefinitionPair(data, root);
        if (ImmutablePair.nullPair().equals(redefinitionPair)) return data;
        Record redefinition = sdk.findNodes(nodeQualifier.dataNodeSearchSpec(redefinitionPair.getKey())).getFirst();
        Record redefinedRecord = sdk.findNodes(nodeQualifier.dataNodeSearchSpec(redefinitionPair.getValue())).getFirst();
        sdk.connect(redefinition, redefinedRecord, REDEFINES);
        return data;
    }

}

