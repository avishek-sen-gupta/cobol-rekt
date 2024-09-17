package org.smojol.toolkit.analysis.graph.neo4j;

import com.mojo.woof.GraphSDK;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.neo4j.driver.Record;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.graph.graphml.Neo4JASTBuilderVisitor;
import org.smojol.toolkit.analysis.graph.graphml.Neo4JDataDependencyBuilderVisitor;
import org.smojol.toolkit.analysis.pipeline.config.GraphBuildConfig;
import org.smojol.common.ast.FlowNodeASTTraversal;

public class Neo4JGraphBuilder {
    private final GraphSDK sdk;
    private final CobolDataStructure data;
    private final NodeSpecBuilder qualifier;
    private final NodeReferenceStrategy astNodeReferenceStrategy;
    private final NodeReferenceStrategy dependencyAttachmentStrategy;
    private Graph<FlowNode, DefaultEdge> graph;

    public Neo4JGraphBuilder(GraphSDK sdk, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, GraphBuildConfig graphBuildConfig) {
        this.sdk = sdk;
        this.data = dataStructures;
        this.qualifier = qualifier;
        this.astNodeReferenceStrategy = graphBuildConfig.astNodeReferenceStrategy();
        this.dependencyAttachmentStrategy = graphBuildConfig.dataDependencyAttachmentStrategy();
    }

    public void buildAST(FlowNode node) {
//        new FlowNodeASTTraversal<Record>().build(node, this::make);
        new FlowNodeASTTraversal<Record>().accept(node, new Neo4JASTBuilderVisitor(astNodeReferenceStrategy, sdk, qualifier));
    }

    public void buildDataDependencies(FlowNode root) {
//        new FlowNodeASTTraversal<Boolean>().build(root, this::buildDataDependency);
        new FlowNodeASTTraversal<Record>().accept(root, new Neo4JDataDependencyBuilderVisitor(data, sdk, qualifier, dependencyAttachmentStrategy));
    }
}
