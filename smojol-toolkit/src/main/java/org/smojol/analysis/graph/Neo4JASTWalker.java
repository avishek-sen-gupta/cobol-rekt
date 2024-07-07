package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.NodeLabels;
import com.mojo.woof.NodeProperties;
import com.mojo.woof.WoofNode;
import org.neo4j.driver.Record;
import org.smojol.ast.MoveFlowNode;
import org.smojol.common.flowchart.*;
import org.smojol.common.vm.reference.CobolReference;
import org.smojol.common.vm.reference.ReferenceBuilder;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.navigation.FlowNodeASTTraversal;

import java.util.Map;
import java.util.UUID;

import static com.mojo.woof.NodeLabels.AST_NODE;
import static com.mojo.woof.NodeProperties.*;
import static com.mojo.woof.NodeRelations.CONTAINS;
import static com.mojo.woof.NodeRelations.MODIFIED_BY;
import static org.smojol.analysis.graph.NodeToWoof.dataStructureToWoof;

public class Neo4JASTWalker {
    private final GraphSDK sdk;
    private final CobolDataStructure data;

    public Neo4JASTWalker(GraphSDK sdk, CobolDataStructure dataStructures) {
        this.sdk = sdk;
        this.data = dataStructures;
    }

    public void buildAST(FlowNode node) {
        new FlowNodeASTTraversal<Record>().build(node, this::make);
    }

    public Record make(FlowNode tree, Record parent) {
        WoofNode node = new WoofNode(Map.of(FLOW_ID, UUID.randomUUID().toString(),
                TEXT, NodeText.originalText(tree.getExecutionContext(), NodeText::PASSTHROUGH),
                TYPE, tree.type().toString()),
                ImmutableList.of(AST_NODE, tree.type().toString()));
        Record record = sdk.createNode(node);
        if (parent == null) return record;
        sdk.connect(parent, record, CONTAINS);
        return record;
    }

    public Boolean buildDataDependency(FlowNode node, Boolean parent) {
        if (node.type() != FlowNodeType.MOVE
         && node.type() != FlowNodeType.COMPUTE
         && node.type() != FlowNodeType.ADD
         && node.type() != FlowNodeType.SUBTRACT
         && node.type() != FlowNodeType.MULTIPLY
         && node.type() != FlowNodeType.DIVIDE
        ) return false;

        if (node.type() == FlowNodeType.MOVE) {
            MoveFlowNode move = (MoveFlowNode) node;
            ReferenceBuilder referenceBuilder = new ReferenceBuilder();
            CobolDataStructure from = referenceBuilder.getReference(move.getFrom(), data).resolve();
            Record n4jFrom = sdk.newOrExisting(ImmutableList.of(NodeLabels.DATA_STRUCTURE), Map.of(NAME, from.name()), dataStructureToWoof(from));

            move.getTos().forEach(to -> {
                CobolDataStructure toDS = referenceBuilder.getReference(to, data).resolve();
                Record n4jTo = sdk.findNode(ImmutableList.of(NodeLabels.DATA_STRUCTURE), Map.of(NAME, toDS.name())).getFirst();
                sdk.connect(n4jTo, n4jFrom, MODIFIED_BY);
            });
        }
        return true;
    }

    private Boolean stopAtSentence(FlowNode tree) {
        return tree.type() == FlowNodeType.SECTION;
    }

    public void buildDataDependencies(FlowNode root) {
        new FlowNodeASTTraversal<Boolean>().build(root, this::buildDataDependency);
    }
}
