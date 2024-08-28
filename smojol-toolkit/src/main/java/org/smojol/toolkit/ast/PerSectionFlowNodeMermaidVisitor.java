package org.smojol.toolkit.ast;

import lombok.Getter;
import org.apache.commons.text.StringEscapeUtils;
import org.smojol.common.ast.*;

import java.util.ArrayList;
import java.util.List;

import static guru.nidi.graphviz.model.Factory.mutGraph;
import static guru.nidi.graphviz.model.Factory.mutNode;

public class PerSectionFlowNodeMermaidVisitor implements FlowNodeVisitor {
    @Getter private final ArrayList<String> lines = new ArrayList<>();
    private final FlowNode root;

    public PerSectionFlowNodeMermaidVisitor(FlowNode root) {
        this.root = root;
    }

    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext visitContext, FlowNodeService nodeService) {
        if (node.type() == FlowNodeType.SECTION) return;
        lines.addAll(outgoingNodes.stream().map(o -> directedPeerEdge(node, o)).toList());
    }

    private static String node(FlowNode node) {
        return String.format("%s[\"%s\"]", node.id(), escaped(node.originalText()));
    }

    private static String escaped(String s) {
        return StringEscapeUtils.escapeHtml4(s);
//        return s.replace("\"", "#quot;");
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext visitContext, FlowNodeService nodeService) {
        visitParentChildLink(parent, internalTreeRoot, visitContext, nodeService, FlowNodeCondition.ALWAYS_SHOW);
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext visitContext, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
        lines.add(directedParentChildEdge(parent, internalTreeRoot));
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {
        lines.add(directedJumpEdge(from, to));
    }

    private String directedParentChildEdge(FlowNode from, FlowNode to) {
        return styledEdge(from, to, "-.->");
    }

    private String directedJumpEdge(FlowNode from, FlowNode to) {
        return styledEdge(from, to, "==>");
    }

    private String directedPeerEdge(FlowNode from, FlowNode to) {
        return styledEdge(from, to, "-->");
    }

    private static String styledEdge(FlowNode from, FlowNode to, String edgeStyle) {
        return node(from) + " " + edgeStyle + " " + node(to);
    }

    @Override
    public FlowNodeVisitor newScope(FlowNode enclosingScope) {
        return this;
    }

    @Override
    public void group(FlowNode root) {
    }
}
