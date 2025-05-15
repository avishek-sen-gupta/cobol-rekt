package com.mojo.algorithms.transpiler;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.mojo.algorithms.domain.SemanticCategory;
import com.mojo.algorithms.domain.TranspilerNode;

import java.util.List;
import java.util.Map;

import static org.apache.commons.lang3.StringUtils.truncate;

public class TranspilerCodeBlockNode extends TranspilerNode {
    public TranspilerCodeBlockNode(List<TranspilerNode> children) {
        super(children, ImmutableMap.of(), ImmutableList.of(SemanticCategory.CODE_BLOCK));
    }

    public TranspilerCodeBlockNode() {
        this(ImmutableList.of());
    }

    public TranspilerCodeBlockNode(TranspilerNode single) {
        super(ImmutableList.of(single), ImmutableMap.of(), ImmutableList.of(SemanticCategory.CODE_BLOCK));
    }

    public TranspilerCodeBlockNode(List<TranspilerNode> childTranspilerNodes, Map<String, Object> additionalAttributes) {
        super(childTranspilerNodes, additionalAttributes, ImmutableList.of(SemanticCategory.CODE_BLOCK));
    }

    public TranspilerCodeBlockNode(List<TranspilerNode> children, Map<String, Object> properties, List<SemanticCategory> categories) {
        super(children, properties, categories);
    }

    public boolean isEmpty() {
        return childTranspilerNodes.isEmpty();
    }

    public TranspilerNode unwrap() {
        return childTranspilerNodes.size() != 1 ? this : childTranspilerNodes.getFirst();
    }

    @Override
    public String description() {
//        return "CODE_BLOCK: " + id;
        return "CODE_BLOCK: " + " {\n" + String.join("\n", childTranspilerNodes.stream().map(TranspilerNode::description).toList()) + "\n}\n";
    }

    public void add(TranspilerNode node) {
        childTranspilerNodes.add(node);
    }

    @Override
    public String shortDescription() {
        return truncate(description(), 40);
    }
}
