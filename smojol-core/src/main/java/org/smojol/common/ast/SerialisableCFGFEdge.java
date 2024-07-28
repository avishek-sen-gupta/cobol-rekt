package org.smojol.common.ast;

public record SerialisableCFGFEdge(
        String id,
        String fromNodeID,
        String toNodeID,
        String edgeType) {
}

