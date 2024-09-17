package org.smojol.common.transpiler;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.SemanticCategory;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class TranspilerNode {
    protected Map<String, Object> properties = new HashMap<>();
    private List<SemanticCategory> categories;

    public TranspilerNode(List<SemanticCategory> categories) {
        this.categories = categories;
    }

    @Override
    public String toString() {
        return description();
    }

    public abstract String description();
    public <T> T getProperty(String key) {
        return (T) properties.get(key);
    }

    public Collection<TranspilerNode> astChildren() {
        return ImmutableList.of();
    }
}
