package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import lombok.Getter;
import org.smojol.common.ast.SemanticCategory;
import org.smojol.common.id.Identifiable;
import org.smojol.common.navigation.TreeNode;

import java.util.*;

public abstract class TranspilerNode implements Identifiable, TreeNode {
    protected final Map<String, Object> properties;
    @Getter private final List<SemanticCategory> categories;
    protected final List<TranspilerNode> childTranspilerNodes = new ArrayList<>();
    protected final String id;

    public TranspilerNode(List<SemanticCategory> categories) {
        this(ImmutableList.of(), Map.of(), categories);
    }

    protected TranspilerNode(List<TranspilerNode> childTranspilerNodes, Map<String, Object> additionalAttributes, List<SemanticCategory> categories) {
        this.childTranspilerNodes.addAll(childTranspilerNodes);
        this.properties = additionalAttributes;
        this.categories = categories;
        this.id = UUID.randomUUID().toString();
    }

    public TranspilerNode(List<TranspilerNode> childTranspilerNodes, List<SemanticCategory> categories) {
        this(childTranspilerNodes, ImmutableMap.of(), categories);
    }

    @Override
    public String id() {
        return id;
    }

    @Override
    public String label() {
        return description();
    }

    @Override
    public String toString() {
        return description();
    }

    public abstract String description();

    public <T> T getProperty(String key) {
        if (!properties.containsKey(key)) return null;
        return (T) properties.get(key);
    }

    public <T> void setProperty(String key, T value) {
        properties.put(key, value);
    }

    public Collection<TranspilerNode> astChildren() {
        return childTranspilerNodes;
    }

    public Collection<TranspilerNode> internalElements() {
        return ImmutableList.of();
    }

    public String shortDescription() {
        return description();
    }

    public boolean replace(TranspilerNode replacedNode, List<TranspilerNode> replacementNodes) {
        int i = childTranspilerNodes.indexOf(replacedNode);
        if (i == -1) return false;
        childTranspilerNodes.remove(replacedNode);
        childTranspilerNodes.addAll(i, replacementNodes);
        return true;
    }

    public boolean addAfter(TranspilerNode targetNode, List<TranspilerNode> successors) {
        int i = childTranspilerNodes.indexOf(targetNode);
        if (i == -1) return false;
        childTranspilerNodes.addAll(i + 1, successors);
        return true;
    }

    public boolean replaceToEnd(TranspilerNode from, List<TranspilerNode> replacingNodes) {
        int indexToRemoveFrom = childTranspilerNodes.indexOf(from);
        if (indexToRemoveFrom == -1) return false;
        while (childTranspilerNodes.size() > indexToRemoveFrom) childTranspilerNodes.removeLast();
        childTranspilerNodes.addAll(replacingNodes);
        return true;
    }

    public List<TranspilerNode> everythingAfter(TranspilerNode node) {
        int fromIndex = childTranspilerNodes.indexOf(node);
        if (fromIndex == -1) return ImmutableList.of();
        if (fromIndex == childTranspilerNodes.size() - 1) return ImmutableList.of();
        return childTranspilerNodes.subList(fromIndex + 1, childTranspilerNodes.size());
    }

    public List<TranspilerNode> range(TranspilerNode from, TranspilerNode to) {
        int fromIndex = childTranspilerNodes.indexOf(from);
        if (fromIndex == -1) return ImmutableList.of();
        int toIndex = childTranspilerNodes.indexOf(to);
        if (toIndex == -1) return ImmutableList.of();
        return childTranspilerNodes.subList(fromIndex, toIndex + 1);
    }
}
