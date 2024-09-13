package org.smojol.toolkit.ast;

import lombok.Getter;
import org.antlr.v4.runtime.ParserRuleContext;
import org.apache.commons.text.StringEscapeUtils;
import org.smojol.common.ast.*;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.util.ArrayList;
import java.util.List;

public class PerSectionFlowNodeMermaidVisitor implements FlowNodeVisitor {
    @Getter
    private final List<String> lines = new ArrayList<>();
    private final FlowNode root;
    private final CobolEntityNavigator navigator;

    public PerSectionFlowNodeMermaidVisitor(FlowNode root) {
        this.root = root;
        navigator = new CobolEntityNavigator((ParserRuleContext) root.getExecutionContext());
    }

    public void visit(FlowNode node, List<FlowNode> outgoingNodes, List<FlowNode> incomingNodes, VisitContext visitContext, FlowNodeService nodeService) {
        if (node.type() == FlowNodeType.SECTION && node != root) return;
        if (node.type() == FlowNodeType.SECTION && node == root) {
            lines.add(node(node));
            return;
        }

        if (!contains(node)) return;
        lines.addAll(outgoingNodes.stream().map(o -> directedPeerEdge(node, o)).toList());
    }

    private static String node(FlowNode node) {
        return switch (node.type()) {
            case SENTENCE, SECTION_HEADER, DIALECT, COMPOSITE, DUMMY, PARAGRAPH, PARAGRAPHS, PARAGRAPH_NAME, SYMBOL,
                 IF_YES, IF_NO, CONDITIONAL_STATEMENT ->
                    String.format("%s(( ))", node.id());
            case SECTION, PROCEDURE_DIVISION_BODY -> String.format("%s[\"%s\"]", node.id(), escaped(node.name()));
            case IF_BRANCH, ON_CLAUSE -> String.format("%s{\"%s\"}", node.id(), escaped(node.label()));
            case CONTROL_FLOW -> String.format("%s((\"%s\"))", node.id(), escaped(node.label()));
            default -> String.format("%s[\"%s\"]", node.id(), escaped(node.originalText()));
        };
    }

    private static String escaped(String s) {
        return StringEscapeUtils.escapeHtml4(s).replace("\n", "<br>");
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode internalTreeRoot, VisitContext visitContext, FlowNodeService nodeService) {
        visitParentChildLink(parent, internalTreeRoot, visitContext, nodeService, FlowNodeCondition.ALWAYS_SHOW);
    }

    @Override
    public void visitParentChildLink(FlowNode parent, FlowNode child, VisitContext visitContext, FlowNodeService nodeService, FlowNodeCondition hideStrategy) {
        if (!contains(parent)) return;
        if (parent.type() == FlowNodeType.IF_BRANCH)
            lines.add(directedLabelledParentChildEdge(parent, child, child.type() == FlowNodeType.IF_YES ? "YES" : "NO"));
        else lines.add(directedParentChildEdge(parent, child));
    }

    private boolean contains(FlowNode node) {
        return navigator.findByCondition(n -> n == node.getExecutionContext()) != null;
    }

    private String directedLabelledParentChildEdge(FlowNode parent, FlowNode child, String label) {
        return styledEdge(parent, child, "-- " + label + " -->");
    }

    @Override
    public void visitControlTransfer(FlowNode from, FlowNode to, VisitContext visitContext) {
        if (!contains(from)) return;
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
