package org.smojol.common.graph;

import lombok.Getter;
import org.smojol.common.id.Identifiable;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static org.smojol.common.graph.DepthFirstTraversalLabelTask.DFS_DISCOVERY_START;

public class CodeGraphNode<V extends Identifiable> implements Identifiable {
    private final Map<String, Object> properties = new HashMap<>();
    @Getter private final String id;
    @Getter private final V originalNode;

    public CodeGraphNode(V originalNode) {
        this.originalNode = originalNode;
        this.id = originalNode.id();
    }

    @Override
    public String id() {
        return id;
    }

    @Override
    public String label() {
        return id;
    }

    public <T> T getProperty(String key, Class<T> type) {
        if (!properties.containsKey(key)) return null;
        return (T) properties.get(key);
    }

    public <T> void setProperty(String key, T value) {
        properties.put(key, value);
    }

    @Override
    public String toString() {
        return getProperty(DFS_DISCOVERY_START, Integer.class).toString();
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
