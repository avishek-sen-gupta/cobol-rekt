package org.smojol.toolkit.analysis.pipeline.config;

import org.smojol.toolkit.analysis.graph.neo4j.NodeReferenceStrategy;

public record GraphBuildConfig(NodeReferenceStrategy astNodeReferenceStrategy, NodeReferenceStrategy dataDependencyAttachmentStrategy) {
}
