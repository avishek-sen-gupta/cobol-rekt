package org.smojol.analysis.graph.neo4j;

import com.mojo.woof.GraphSDK;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.neo4j.driver.Record;
import org.smojol.analysis.graph.DataDependencyPairComputer;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.analysis.graph.NodeToWoof;
import org.smojol.common.flowchart.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.navigation.FlowNodeASTTraversal;

import java.util.List;
import java.util.Map;

public class Neo4JASTExporter {
    private final GraphSDK sdk;
    private final CobolDataStructure data;
    private final NodeSpecBuilder qualifier;
    private final NodeReferenceStrategy astNodeReferenceStrategy;
    private final NodeReferenceStrategy dependencyAttachmentStrategy;
    private Graph<FlowNode, DefaultEdge> graph;

    public Neo4JASTExporter(GraphSDK sdk, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, NodeReferenceStrategy nodeReferenceStrategy, NodeReferenceStrategy dependencyAttachmentStrategy) {
        this.sdk = sdk;
        this.data = dataStructures;
        this.qualifier = qualifier;
        this.astNodeReferenceStrategy = nodeReferenceStrategy;
        this.dependencyAttachmentStrategy = dependencyAttachmentStrategy;
    }

    public void buildAST(FlowNode node) {
        new FlowNodeASTTraversal<Record>().build(node, this::make);
    }

    public void buildDataDependencies(FlowNode root) {
        new FlowNodeASTTraversal<Boolean>().build(root, this::buildDataDependency);
    }

    public Record make(FlowNode tree, Record parent) {
        Record record = astNodeReferenceStrategy.reference(tree, sdk, qualifier);
//        WoofNode node = new WoofNode(qualifier.newASTNode(tree));
//        Record record = sdk.createNode(node);
//        Record record = findNode(tree, sdk, qualifier);
        if (parent == null) return record;
        sdk.contains(parent, record);
        return record;
    }

    public Boolean buildDataDependency(FlowNode node, Boolean parent) {
        Map.Entry<List<CobolDataStructure>, List<CobolDataStructure>> pairs = DataDependencyPairComputer.dependencyPairs(node, data);
        if (ImmutablePair.nullPair().equals(pairs)) return false;
        connect(pairs.getKey(), pairs.getValue(), node);
        return true;
    }

    private void connect(List<CobolDataStructure> froms, List<CobolDataStructure> tos, FlowNode cfgNode) {
//        Record cfgNodeRecord = sdk.findNodes(qualifier.cfgNodeSearchSpec(cfgNode)).getFirst();
        Record cfgNodeRecord = dependencyAttachmentStrategy.reference(cfgNode, sdk, qualifier);
        tos.forEach(to -> froms.forEach(from -> {
            Record n4jTo = sdk.findNodes(qualifier.dataNodeSearchSpec(to)).getFirst();
            Record n4jFrom = sdk.newOrExisting(qualifier.dataNodeSearchSpec(from), NodeToWoof.dataStructureToWoof(from, qualifier));
            sdk.flowsInto(n4jFrom, n4jTo);
            sdk.modifies(cfgNodeRecord, n4jTo);
            sdk.accesses(cfgNodeRecord, n4jFrom);
        }));
    }

    private Boolean stopAtSentence(FlowNode tree) {
        return tree.type() == FlowNodeType.SECTION;
    }
}
