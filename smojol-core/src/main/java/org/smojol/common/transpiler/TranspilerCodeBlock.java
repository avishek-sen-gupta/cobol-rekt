package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.smojol.common.ast.SemanticCategory;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import static org.apache.commons.lang3.StringUtils.truncate;

public class TranspilerCodeBlock extends TranspilerNode {
    public TranspilerCodeBlock(List<TranspilerNode> children) {
        super(children, ImmutableMap.of(), ImmutableList.of(SemanticCategory.CODE_BLOCK));
    }

    public TranspilerCodeBlock() {
        this(ImmutableList.of());
    }

    public TranspilerCodeBlock(TranspilerNode single) {
        super(ImmutableList.of(single), ImmutableMap.of(), ImmutableList.of(SemanticCategory.CODE_BLOCK));
    }

    public TranspilerCodeBlock(List<TranspilerNode> childTranspilerNodes, Map<String, Object> additionalAttributes) {
        super(childTranspilerNodes, additionalAttributes, ImmutableList.of(SemanticCategory.CODE_BLOCK));
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
        return truncate("CODE_BLOCK: " + String.join("\n", childTranspilerNodes.stream().map(TranspilerNode::description).toList()), 70);
    }

    @Override
    public Collection<TranspilerNode> astChildren() {
        return childTranspilerNodes;
    }

    public void add(TranspilerNode node) {
        childTranspilerNodes.add(node);
    }
}
