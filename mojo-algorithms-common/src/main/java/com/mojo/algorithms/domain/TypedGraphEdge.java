package com.mojo.algorithms.domain;

import lombok.Getter;
import org.jgrapht.graph.DefaultEdge;

@Getter
public class TypedGraphEdge extends DefaultEdge {
    private final String relationshipType;
    private final String namespace;

    public TypedGraphEdge(String relationshipType, String namespace) {
        this.relationshipType = relationshipType;
        this.namespace = namespace;
    }
}
