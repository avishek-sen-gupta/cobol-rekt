package org.smojol.analysis.graph;

import lombok.Getter;

public class TypedGraphMLEdge extends org.jgrapht.graph.DefaultEdge {
    @Getter private final String relationshipType;

    public TypedGraphMLEdge(String relationshipType) {
        this.relationshipType = relationshipType;
    }
}
