package org.smojol.toolkit.intermediate;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.CodeSentinelType;
import com.mojo.algorithms.graph.NamespaceQualifier;
import com.mojo.algorithms.graph.TypedCodeVertex;
import com.mojo.algorithms.graph.TypedGraphEdge;
import com.mojo.algorithms.graph.TypedGraphVertex;
import com.mojo.algorithms.id.UUIDProvider;
import com.mojo.algorithms.transpiler.FlowNodeLike;
import com.mojo.algorithms.transpiler.TranspilerNode;
import com.mojo.woof.NodeSpec;
import org.smojol.common.program.CobolProgram;
import org.smojol.common.ast.CommentBlock;
import com.mojo.algorithms.id.IdProvider;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.graph.graphml.TypedDataStructureVertex;

import java.util.HashMap;
import java.util.Map;

import static com.mojo.woof.NodeLabels.*;
import static com.mojo.woof.NodeProperties.*;
import static com.mojo.woof.NodeProperties.LEVEL;
import static com.mojo.algorithms.transpiler.SemanticCategory.METADATA;
import static com.mojo.algorithms.transpiler.SemanticCategory.PROGRAM;

// TODO: Move to common
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
        return new NodeSpec(ImmutableList.of(DATA_STRUCTURE, structure.getDataType().abstractType().name()),
                Map.of(ID, idProvider.next(),
                        INTERNAL_ID, structure.getId(),
                        NAME, structure.name(),
                        TEXT, structure.content(),
                        TYPE, structure.getDataType().abstractType().name(),
                        ENTITY_TYPE, DATA_STRUCTURE,
                        ENTITY_CATEGORIES, ImmutableList.of(structure.dataCategory().name()),
                        LEVEL, structure.getLevelNumber(),
                        SECTION_SOURCE, structure.getSourceSection().name(),
                        NAMESPACE, namespaceQualifier.getNamespace()
                ));
    }

    public NodeSpec newASTNode(FlowNodeLike node) {
        return labelledCodeNode(node, AST_NODE);
    }

    public NodeSpec newCFGNode(FlowNodeLike node) {
        return labelledCodeNode(node, CFG_NODE);
    }

    public NodeSpec labelledCodeNode(FlowNodeLike node, String nodeType) {
        return new NodeSpec(ImmutableList.of(nodeType, node.type().toString()),
                Map.of(ID, idProvider.next(),
                        INTERNAL_ID, node.id(),
                        NAME, node.name(),
                        TEXT, node.originalText(),
                        TYPE, node.type().toString(),
                        ENTITY_TYPE, nodeType,
                        ENTITY_CATEGORIES, node.categories().stream().map(Enum::name).toList(),
                        CODE_SENTINEL_TYPE, node.codeSentinelType().name(),
                        NAMESPACE, namespaceQualifier.getNamespace()
                ));
    }

    public NodeSpec newTraceNode(FlowNodeLike node) {
        return labelledCodeNode(node, CFG_TRACE);
    }

    public NodeSpec dataNodeSearchSpec(CobolDataStructure structure) {
        return dataNodeSearchCriteria(Map.of(INTERNAL_ID, structure.getId(),
                NAME, structure.name(),
                ENTITY_CATEGORIES, ImmutableList.of(structure.dataCategory().name()),
                NAMESPACE, namespaceQualifier.getNamespace()));
    }

    public NodeSpec cfgNodeSearchSpec(FlowNodeLike node) {
        return labelledNodeSearchSpec(node, CFG_NODE);
    }

    public NodeSpec astNodeSearchSpec(FlowNodeLike node) {
        return labelledNodeSearchSpec(node, AST_NODE);
    }

    public NodeSpec labelledNodeSearchSpec(FlowNodeLike node, String nodeType) {
        return new NodeSpec(ImmutableList.of(nodeType), Map.of(INTERNAL_ID, node.id(), ENTITY_TYPE, nodeType, NAMESPACE, namespaceQualifier.getNamespace()));
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

    public NodeSpec cfgNodeCriteria(Map<String, Object> criteria) {
        HashMap<String, Object> finalCriteria = new HashMap<>();
        finalCriteria.put(NAMESPACE, namespaceQualifier.getNamespace());
        finalCriteria.putAll(criteria);
        return new NodeSpec(ImmutableList.of(CFG_NODE), finalCriteria);
    }

    public TypedGraphVertex newCodeVertex(FlowNodeLike node) {
        return new TypedCodeVertex(node, namespaceQualifier.getNamespace());
    }

    public TypedGraphEdge newEdge(String edgeType) {
        return new TypedGraphEdge(edgeType, namespaceQualifier.getNamespace());
    }

    public TypedGraphVertex newDataVertex(CobolDataStructure structure) {
        return new TypedDataStructureVertex(structure, namespaceQualifier.getNamespace());
    }

    public NodeSpec commentNode(CommentBlock comment) {
        String id = idProvider.next();
        return new NodeSpec(ImmutableList.of(COMMENT_NODE),
                Map.of(ID, id,
                        INTERNAL_ID, id,
                        NAME, COMMENT_NODE,
                        TEXT, comment.toString(),
                        TYPE, COMMENT_NODE,
                        ENTITY_TYPE, COMMENT_NODE,
                        ENTITY_CATEGORIES, ImmutableList.of(METADATA.name()),
                        CODE_SENTINEL_TYPE, CodeSentinelType.BODY.name(),
                        NAMESPACE, namespaceQualifier.getNamespace()
                ));
    }

    public NodeSpec program(CobolProgram program) {
        String id = idProvider.next();
        return new NodeSpec(ImmutableList.of(PROGRAM_NODE),
                Map.of(ID, id,
                        INTERNAL_ID, id,
                        NAME, program.getName(),
                        TEXT, "",
                        TYPE, program.getCallTarget().getProgramReferenceType().name(),
                        ENTITY_TYPE, PROGRAM_NODE,
                        ENTITY_CATEGORIES, ImmutableList.of(PROGRAM.name()),
                        NAMESPACE, namespaceQualifier.getNamespace()
                ));
    }

    public NodeSpec newTranspilerNode(TranspilerNode node) {
        return new NodeSpec(ImmutableList.of("TRANSPILER_NODE"),
                Map.of(ID, idProvider.next(),
                        INTERNAL_ID, node.id(),
                        NAME, node.label(),
                        TEXT, node.description(),
                        TYPE, "TRANSPILER_NODE",
                        ENTITY_TYPE, "TRANSPILER_NODE",
                        ENTITY_CATEGORIES, node.getCategories().stream().map(Enum::name).toList(),
                        CODE_SENTINEL_TYPE, "BODY",
                        NAMESPACE, namespaceQualifier.getNamespace()
                ));
    }
}
