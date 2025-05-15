package com.mojo.algorithms.domain;

import org.jgrapht.graph.DefaultEdge;

import java.util.Map;

public class AnnotatedEdge extends DefaultEdge {
    private final Map<String, String> metadata;

    public AnnotatedEdge(Map<String, String> metadata) {
        this.metadata = metadata;
    }

    @Override
    public Object clone() {
        return new AnnotatedEdge(metadata);
    }

    public String data(String key) {
        return metadata.get(key);
    }
}
