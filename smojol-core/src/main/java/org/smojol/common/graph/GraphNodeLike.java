package org.smojol.common.graph;

public interface GraphNodeLike {
    String id();
    <T> T getProperty(String key, Class<T> type);

    <T> void setProperty(String key, T value);
}
