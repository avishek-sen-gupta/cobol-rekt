package org.smojol.common.graph;

import java.util.HashMap;
import java.util.Map;

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
        return getProperty("DFS_NUM", Integer.class).toString();
    }
}
