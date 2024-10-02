package org.smojol.common.graph;

import org.smojol.common.id.Identifiable;

public interface GraphNodeLike extends Identifiable {
    String id();
    <T> T getProperty(String key, Class<T> type);
    <T> void setProperty(String key, T value);
}
