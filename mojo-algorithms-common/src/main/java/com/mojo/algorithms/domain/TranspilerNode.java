package com.mojo.algorithms.domain;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.mojo.algorithms.id.Identifiable;
import com.mojo.algorithms.navigation.GenericTreeNode;
import com.mojo.algorithms.transpiler.TranspilerTreeFormatter;
import lombok.Getter;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Stream;

public abstract class TranspilerNode implements Identifiable, GenericTreeNode<TranspilerNode> {
    private final String nodeType = this.getClass().getSimpleName();
    protected final Map<String, Object> properties;
    @Getter
    private final List<SemanticCategory> categories;
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
        return new TranspilerTreeFormatter().format(this);
    }

    public abstract String description();

    public <T> T getProperty(String key) {
        if (!properties.containsKey(key)) return null;
        return (T) properties.get(key);
    }

    public List<TranspilerNode> astChildren() {
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

    public List<TranspilerNode> range(TranspilerNode fromInclusive, TranspilerNode toInclusive) {
        int fromIndex = childTranspilerNodes.indexOf(fromInclusive);
        if (fromIndex == -1) return ImmutableList.of();
        int toIndex = childTranspilerNodes.indexOf(toInclusive);
        if (toIndex == -1) return ImmutableList.of();
        return new ArrayList<>(childTranspilerNodes.subList(fromIndex, toIndex + 1));
    }

    public Optional<TranspilerNode> findOne(Predicate<TranspilerNode> matchCondition) {
        return childTranspilerNodes.stream().filter(matchCondition).findFirst();
    }

    public boolean replaceRangeToInclusive(Pair<TranspilerNode, TranspilerNode> range, List<TranspilerNode> replacingNodes) {
        List<TranspilerNode> r = range(range.getLeft(), range.getRight());
        if (r.isEmpty()) return false;
        boolean replacedSuccessfully = replace(r.getFirst(), replacingNodes);
        r.forEach(childTranspilerNodes::remove);
        return replacedSuccessfully;
    }


    public boolean replaceRangeToExclusive(Pair<TranspilerNode, TranspilerNode> range, List<TranspilerNode> replacingNodes) {
        List<TranspilerNode> r = range(range.getLeft(), range.getRight());
        if (r.size() <= 1) return false;
        return replaceRangeToInclusive(ImmutablePair.of(range.getLeft(), r.get(r.size() - 2)), replacingNodes);
    }

    public List<TranspilerNode> findAllRecursive(Predicate<TranspilerNode> p) {
        return findAllRecursive_(this, p).toList();
    }

    public <T extends TranspilerNode> List<T> allOfType(Class<T> nodeType) {
        return findAllRecursive(t -> t.getClass() == nodeType).stream().map(n -> (T) n).toList();
    }

    private Stream<TranspilerNode> findAllRecursive_(TranspilerNode node, Predicate<TranspilerNode> p) {
        List<TranspilerNode> parent = p.test(node) ? ImmutableList.of(node) : ImmutableList.of();
        return Stream.concat(parent.stream(), node.astChildren().stream().flatMap(c -> findAllRecursive_(c, p)));
    }
}
