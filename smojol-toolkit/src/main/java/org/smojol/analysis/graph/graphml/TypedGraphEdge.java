package org.smojol.analysis.graph.graphml;

import lombok.Getter;

@Getter
public class TypedGraphEdge extends org.jgrapht.graph.DefaultEdge {
    private final String relationshipType;

    public TypedGraphEdge(String relationshipType) {
        this.relationshipType = relationshipType;
    }
}
