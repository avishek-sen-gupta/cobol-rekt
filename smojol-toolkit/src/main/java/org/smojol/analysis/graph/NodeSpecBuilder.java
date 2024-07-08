package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import com.mojo.woof.NodeSpec;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.NodeText;
import org.smojol.common.id.IdProvider;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.HashMap;
import java.util.Map;

import static com.mojo.woof.NodeLabels.*;
import static com.mojo.woof.NodeProperties.*;
import static com.mojo.woof.NodeProperties.LEVEL;

public class NodeSpecBuilder {
    private final NamespaceQualifier namespaceQualifier;
    private final IdProvider idProvider;

    public NodeSpecBuilder(NamespaceQualifier namespaceQualifier) {
        this(namespaceQualifier, new UUIDProvider());
    }

    public NodeSpecBuilder(NamespaceQualifier namespaceQualifier, IdProvider idProvider) {
        this.namespaceQualifier = namespaceQualifier;
        this.idProvider = idProvider;
    }

    public NodeSpec newDataNode(CobolDataStructure structure) {
        return new NodeSpec(ImmutableList.of(DATA_STRUCTURE, structure.getDataType().toString()),
                Map.of(ID, idProvider.next(),
                        HUMAN_READABLE_ID, structure.name(),
                        NAME, structure.name(),
                        TYPE, structure.getDataType().toString(),
                        LEVEL, structure.getLevelNumber(),
                        NAMESPACE, namespaceQualifier.getNamespace()
                ));
    }

    public NodeSpec newASTNode(FlowNode node) {
        return new NodeSpec(ImmutableList.of(AST_NODE, node.type().toString()),
                Map.of(ID, idProvider.next(),
                        HUMAN_READABLE_ID, node.id(),
                        TEXT, NodeText.originalText(node.getExecutionContext(), NodeText::PASSTHROUGH),
                        TYPE, node.type().toString(),
                        NAMESPACE, namespaceQualifier.getNamespace()
                ));
    }

    public NodeSpec newCFGNode(FlowNode node) {
        return new NodeSpec(ImmutableList.of(CFG_NODE, node.type().toString()), Map.of(
                ID, idProvider.next(),
                HUMAN_READABLE_ID, node.id(),
                TEXT, node.getExecutionContext().getText(),
                TYPE, node.type().toString(),
                NAMESPACE, namespaceQualifier.getNamespace()
        ));
    }

    public NodeSpec newTraceNode(FlowNode node) {
        return new NodeSpec(ImmutableList.of(CFG_TRACE, node.type().toString()),
                Map.of(ID, idProvider.next(),
                        HUMAN_READABLE_ID, node.id(),
                        TEXT, node.getExecutionContext().getText(),
                        TYPE, node.type().toString(),
                        NAMESPACE, namespaceQualifier.getNamespace()
                ));
    }

    public NodeSpec dataNodeSearchSpec(CobolDataStructure structure) {
        return dataNodeSearchCriteria(Map.of(NAME, structure.name(), NAMESPACE, namespaceQualifier.getNamespace()));
    }

    public NodeSpec cfgNodeSearchSpec(FlowNode node) {
        return new NodeSpec(ImmutableList.of(CFG_NODE), Map.of(HUMAN_READABLE_ID, node.id(), NAMESPACE, namespaceQualifier.getNamespace()));
    }

    public NodeSpec dataNodeSearchCriteria(Map<String, Object> criteria) {
        HashMap<String, Object> finalCriteria = new HashMap<>();
        finalCriteria.put(NAMESPACE, namespaceQualifier.getNamespace());
        finalCriteria.putAll(criteria);
        return new NodeSpec(ImmutableList.of(DATA_STRUCTURE), finalCriteria);
    }

    public NodeSpec astNodeCriteria(Map<String, Object> criteria) {
        HashMap<String, Object> finalCriteria = new HashMap<>();
        finalCriteria.put(NAMESPACE, namespaceQualifier.getNamespace());
        finalCriteria.putAll(criteria);
        return new NodeSpec(ImmutableList.of(AST_NODE), finalCriteria);
    }
}
