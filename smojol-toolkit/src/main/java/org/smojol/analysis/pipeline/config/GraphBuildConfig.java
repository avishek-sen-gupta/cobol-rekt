package org.smojol.analysis.pipeline.config;

import org.smojol.analysis.graph.neo4j.NodeReferenceStrategy;

public record GraphBuildConfig(NodeReferenceStrategy astNodeReferenceStrategy, NodeReferenceStrategy dataDependencyAttachmentStrategy) {
}
