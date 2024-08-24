package org.smojol.toolkit.analysis.graph.graphml;

import com.mojo.woof.GraphSDK;
import org.neo4j.driver.Record;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeASTVisitor;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.neo4j.NodeReferenceStrategy;

public class Neo4JASTBuilderVisitor extends FlowNodeASTVisitor<Record> {
    private final NodeReferenceStrategy astNodeReferenceStrategy;
    private final GraphSDK graphSDK;
    private final NodeSpecBuilder qualifier;

    public Neo4JASTBuilderVisitor(NodeReferenceStrategy astNodeReferenceStrategy, GraphSDK graphSDK, NodeSpecBuilder qualifier, Record ancestorRecord) {
        super(ancestorRecord);
        this.astNodeReferenceStrategy = astNodeReferenceStrategy;
        this.graphSDK = graphSDK;
        this.qualifier = qualifier;
    }

    public Neo4JASTBuilderVisitor(NodeReferenceStrategy astNodeReferenceStrategy, GraphSDK sdk, NodeSpecBuilder qualifier) {
        this(astNodeReferenceStrategy, sdk, qualifier, null);
    }

    @Override
    public FlowNodeASTVisitor<Record> visit(FlowNode node) {
        Record record = astNodeReferenceStrategy.reference(node, graphSDK, qualifier);
        if (ancestor == null) return new Neo4JASTBuilderVisitor(astNodeReferenceStrategy, graphSDK, qualifier, record);
        graphSDK.containsCodeNode(ancestor, record);
        return new Neo4JASTBuilderVisitor(astNodeReferenceStrategy, graphSDK, qualifier, record);
    }
}
