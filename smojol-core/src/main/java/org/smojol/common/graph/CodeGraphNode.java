package org.smojol.common.graph;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static org.smojol.common.graph.DepthFirstTraversalLabelTask.DFS_NUM;

public class CodeGraphNode implements GraphNodeLike {
    private final Map<String, Object> properties = new HashMap<>();
    private final String id;

    public CodeGraphNode(String id) {
        this.id = id;
    }

    @Override
    public String id() {
        return id;
    }

    @Override
    public String label() {
        return id;
    }

    @Override
    public <T> T getProperty(String key, Class<T> type) {
        if (!properties.containsKey(key)) return null;
        return (T) properties.get(key);
    }

    @Override
    public <T> void setProperty(String key, T value) {
        properties.put(key, value);
    }

    @Override
    public String toString() {
        return getProperty(DFS_NUM, Integer.class).toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CodeGraphNode that)) return false;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
