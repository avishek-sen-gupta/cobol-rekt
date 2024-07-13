package org.smojol.analysis.graph.graphml;

import org.jgrapht.Graph;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.common.flowchart.*;

import java.util.List;

import static com.mojo.woof.NodeRelations.FOLLOWED_BY;
import static com.mojo.woof.NodeRelations.STARTS_WITH;

public class GraphMLCFGVisitor implements FlowNodeVisitor {
    private final Graph<FlowNode, TypedGraphMLEdge> graph;
    private final NodeSpecBuilder qualifier;

    public GraphMLCFGVisitor(Graph<FlowNode, TypedGraphMLEdge> graph, NodeSpecBuilder qualifier) {
        this.graph = graph;
        this.qualifier = qualifier;
    }

    @Override
    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext context, FlowNodeService nodeService) {
        outgoingNodes.forEach(o -> {
            graph.addVertex(node);
            graph.addVertex(o);
            graph.addEdge(node, o, new TypedGraphMLEdge(FOLLOWED_BY));
        });
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService) {
        graph.addVertex(parent);
        graph.addVertex(internalTreeRoot);
        graph.addEdge(parent, internalTreeRoot, new TypedGraphMLEdge(STARTS_WITH));
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
        visitParentChildLink(parent, internalTreeRoot, ctx, nodeService);
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {
        graph.addVertex(from);
        graph.addVertex(to);
        graph.addEdge(from, to, new TypedGraphMLEdge(FOLLOWED_BY));
    }

    @Override
    public FlowNodeVisitor newScope(FlowNode enclosingScope) {
        return this;
    }

    @Override
    public void group(FlowNode root) {

    }
}