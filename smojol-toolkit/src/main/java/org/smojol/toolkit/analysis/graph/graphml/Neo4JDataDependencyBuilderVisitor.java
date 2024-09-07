package org.smojol.toolkit.analysis.graph.graphml;

import com.mojo.woof.GraphSDK;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.neo4j.driver.Record;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeASTVisitor;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.defined.AttachCommentsTask;
import org.smojol.toolkit.analysis.graph.DataDependencyPairComputer;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.NodeToWoof;
import org.smojol.toolkit.analysis.graph.neo4j.NodeReferenceStrategy;

import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

public class Neo4JDataDependencyBuilderVisitor extends FlowNodeASTVisitor<Record> {
    private static final Logger LOGGER = Logger.getLogger(AttachCommentsTask.class.getName());

    private final GraphSDK sdk;
    private final NodeSpecBuilder qualifier;
    private final NodeReferenceStrategy dependencyAttachmentStrategy;
    private final CobolDataStructure dataRoot;

    public Neo4JDataDependencyBuilderVisitor(CobolDataStructure dataRoot, GraphSDK graphSDK, NodeSpecBuilder qualifier, NodeReferenceStrategy dependencyAttachmentStrategy, Record ancestor) {
        super(ancestor);
        this.sdk = graphSDK;
        this.qualifier = qualifier;
        this.dependencyAttachmentStrategy = dependencyAttachmentStrategy;
        this.dataRoot = dataRoot;
    }

    public Neo4JDataDependencyBuilderVisitor(CobolDataStructure dataRoot, GraphSDK sdk, NodeSpecBuilder qualifier, NodeReferenceStrategy dependencyAttachmentStrategy) {
        this(dataRoot, sdk, qualifier, dependencyAttachmentStrategy, null);
    }

    @Override
    public Record visit(FlowNode node) {
        Map.Entry<List<CobolDataStructure>, List<CobolDataStructure>> pairs = DataDependencyPairComputer.dependencyPairs(node, dataRoot);
        if (ImmutablePair.nullPair().equals(pairs)) return null;
        if (pairs.getValue().isEmpty()) {
            accesses(node, pairs.getKey());
            return null;
        }
        connect(pairs.getKey(), pairs.getValue(), node);
        return null;
    }

    @Override
    public FlowNodeASTVisitor<Record> scope(FlowNode n, Record visitResult) {
        return this;
    }

    private void accesses(FlowNode attachmentNode, List<CobolDataStructure> dataNodes) {
        LOGGER.finer("Attaching IF??? " + attachmentNode.type() + " " + dataNodes.size());
        Record attachmentNodeRecord = dependencyAttachmentStrategy.reference(attachmentNode, sdk, qualifier);
        dataNodes.forEach(n -> {
            Record n4jFrom = sdk.newOrExisting(qualifier.dataNodeSearchSpec(n), NodeToWoof.dataStructureToWoof(n, qualifier));
            sdk.accesses(attachmentNodeRecord, n4jFrom);
        });
    }

    private void connect(List<CobolDataStructure> froms, List<CobolDataStructure> tos, FlowNode attachmentNode) {
        Record attachmentNodeRecord = dependencyAttachmentStrategy.reference(attachmentNode, sdk, qualifier);
        tos.forEach(to -> {
            froms.forEach(f -> LOGGER.finer(f.name()));
            List<Record> nodes = sdk.findNodes(qualifier.dataNodeSearchSpec(to));
            Record n4jTo = !nodes.isEmpty() ? nodes.getFirst() : sdk.createNode(NodeToWoof.dataStructureToWoof(to, qualifier));
            sdk.modifies(attachmentNodeRecord, n4jTo);
            froms.forEach(from -> {
                Record n4jFrom = sdk.newOrExisting(qualifier.dataNodeSearchSpec(from), NodeToWoof.dataStructureToWoof(from, qualifier));
                sdk.flowsInto(n4jFrom, n4jTo);
                sdk.accesses(attachmentNodeRecord, n4jFrom);
            });
        });
    }
}
