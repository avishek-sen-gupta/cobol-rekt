package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.smojol.common.ast.SemanticCategory;
import org.smojol.common.id.Identifiable;

import java.util.*;

public abstract class TranspilerNode implements Identifiable {
//    private final String nodeType = getClass().getSimpleName();
    protected final Map<String, Object> properties;
    private final List<SemanticCategory> categories;
    protected final List<TranspilerNode> childTranspilerNodes = new ArrayList<>();
    protected final String id;

    public TranspilerNode(List<SemanticCategory> categories) {
        this(ImmutableList.of(), ImmutableMap.of(), categories);
    }

    protected TranspilerNode(List<TranspilerNode> childTranspilerNodes, Map<String, Object> additionalAttributes, List<SemanticCategory> categories) {
        this.childTranspilerNodes.addAll(childTranspilerNodes);
        this.properties = additionalAttributes;
        this.categories = categories;
        this.id = UUID.randomUUID().toString();
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

    public Collection<TranspilerNode> astChildren() {
        return ImmutableList.of();
    }
}
