package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.NodeSpec;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.NodeText;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.Map;
import java.util.UUID;

import static com.mojo.woof.NodeLabels.*;
import static com.mojo.woof.NodeProperties.*;
import static com.mojo.woof.NodeProperties.LEVEL;

public class NodeSpecBuilder {
    private final NamespaceQualifier namespaceQualifier;

    public NodeSpecBuilder(NamespaceQualifier namespaceQualifier) {
        this.namespaceQualifier = namespaceQualifier;
    }

    public NodeSpec newDataNode(CobolDataStructure structure) {
        return new NodeSpec(ImmutableList.of(DATA_STRUCTURE, structure.getDataType().toString()),
                Map.of(ID, UUID.randomUUID().toString(),
                        NAME, structure.name(),
                        TYPE, structure.getDataType().toString(),
                        LEVEL, structure.getLevelNumber()
                ));
    }

    public NodeSpec newASTNode(FlowNode node) {
        return new NodeSpec(ImmutableList.of(AST_NODE, node.type().toString()),
                Map.of(FLOW_ID, UUID.randomUUID().toString(),
                        TEXT, NodeText.originalText(node.getExecutionContext(), NodeText::PASSTHROUGH),
                        TYPE, node.type().toString()));
    }

    public NodeSpec newCFGNode(FlowNode node) {
        return new NodeSpec(ImmutableList.of(CFG_NODE, node.type().toString()), Map.of(FLOW_ID, node.id(),
                TEXT, node.getExecutionContext().getText(),
                TYPE, node.type().toString()));
    }

    public NodeSpec newTraceNode(FlowNode node) {
        return new NodeSpec(ImmutableList.of(CFG_TRACE, node.type().toString()), Map.of(FLOW_ID, node.id(),
                TEXT, node.getExecutionContext().getText(),
                TYPE, node.type().toString()));
    }

    public NodeSpec dataNodeSearchSpec(CobolDataStructure structure) {
        return dataNodeSearchCriteria(Map.of(NAME, structure.name()));
    }

    public NodeSpec cfgNodeSearchSpec(FlowNode node) {
        return new NodeSpec(ImmutableList.of(CFG_NODE), Map.of(FLOW_ID, node.id()));
    }

    public NodeSpec dataNodeSearchCriteria(Map<String, Object> criteria) {
        return new NodeSpec(ImmutableList.of(DATA_STRUCTURE), criteria);
    }

    public NodeSpec astNodeCriteria(Map<String, Object> criteria) {
        return new NodeSpec(ImmutableList.of(AST_NODE), criteria);
    }
}
