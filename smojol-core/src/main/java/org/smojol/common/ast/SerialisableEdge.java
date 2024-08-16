package org.smojol.common.ast;

public record SerialisableEdge(
        String id,
        String fromNodeID,
        String toNodeID,
        String edgeType) {
}

