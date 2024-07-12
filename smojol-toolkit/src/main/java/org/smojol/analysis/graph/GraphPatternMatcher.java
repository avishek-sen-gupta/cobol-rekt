package org.smojol.analysis.graph;

import com.mojo.woof.GraphSDK;
import lombok.Getter;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.*;

import java.util.ArrayList;
import java.util.List;

public class GraphPatternMatcher implements FlowNodeVisitor {
    private final GraphSDK sdk;
    @Getter
    private final List<List<FlowNode>> matches = new ArrayList<>();
    private List<FlowNode> currentMatch = new ArrayList<>();

    public GraphPatternMatcher(GraphSDK sdk) {
        this.sdk = sdk;
    }

    @Override
    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext context, FlowNodeService nodeService) {
        System.out.printf("Visiting %s%n", node);
        if (node.type() == FlowNodeType.SENTENCE && node.astChildren().size() == 1 && node.astChildren().getFirst().type() == FlowNodeType.MOVE) {
            registerMove(node);
            return;
        }
        if (currentMatch.isEmpty()) return;
        matches.add(currentMatch);
        currentMatch = new ArrayList<>();
    }

    private void registerMove(FlowNode node) {
        currentMatch.add(node);
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService) {
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext ctx, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {
    }

    @Override
    public FlowNodeVisitor newScope(FlowNode enclosingScope) {
        return this;
    }

    @Override
    public void group(FlowNode root) {

    }
}
