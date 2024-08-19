package org.smojol.toolkit.analysis.graph.graphml;

import lombok.Getter;

@Getter
public class TypedGraphEdge extends org.jgrapht.graph.DefaultEdge {
    private final String relationshipType;
    private final String namespace;

    public TypedGraphEdge(String relationshipType, String namespace) {
        this.relationshipType = relationshipType;
        this.namespace = namespace;
    }
}
