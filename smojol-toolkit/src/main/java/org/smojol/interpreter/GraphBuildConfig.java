package org.smojol.interpreter;

import org.smojol.analysis.graph.neo4j.NodeReferenceStrategy;

public record GraphBuildConfig(NodeReferenceStrategy astNodeReferenceStrategy, NodeReferenceStrategy dataDependencyAttachmentStrategy) {
}
