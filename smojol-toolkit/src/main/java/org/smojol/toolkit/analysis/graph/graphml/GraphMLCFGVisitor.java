package org.smojol.toolkit.analysis.graph.graphml;

import com.mojo.algorithms.graph.TypedGraphEdge;
import com.mojo.algorithms.graph.TypedGraphVertex;
import org.jgrapht.Graph;
import org.smojol.toolkit.intermediate.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.jgrapht.JGraphTCodeOperations;
import org.smojol.common.ast.*;

import java.util.List;

import static com.mojo.woof.NodeRelations.FOLLOWED_BY;
import static com.mojo.woof.NodeRelations.STARTS_WITH;

public class GraphMLCFGVisitor implements FlowNodeVisitor {
    private final Graph<TypedGraphVertex, TypedGraphEdge> graph;
    private final NodeSpecBuilder qualifier;
    private final JGraphTCodeOperations operations;

    public GraphMLCFGVisitor(Graph<TypedGraphVertex, TypedGraphEdge> graph, NodeSpecBuilder qualifier) {
        operations = new JGraphTCodeOperations(graph, qualifier);
        this.graph = graph;
        this.qualifier = qualifier;
    }

    @Override
    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext context, FlowNodeService nodeService) {
        outgoingNodes.forEach(o -> {
            operations.addNode(node);
            operations.addNode(o);
            operations.connect(node, o, FOLLOWED_BY);
        });
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService) {
        operations.addNode(parent);
        operations.addNode(internalTreeRoot);
        operations.connect(parent, internalTreeRoot, STARTS_WITH);
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
        visitParentChildLink(parent, internalTreeRoot, ctx, nodeService);
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {
        operations.addNode(from);
        operations.addNode(to);
        operations.connect(from, to, FOLLOWED_BY);
    }

    @Override
    public FlowNodeVisitor newScope(FlowNode enclosingScope) {
        return this;
    }

    @Override
    public void group(FlowNode root) {

    }
}
