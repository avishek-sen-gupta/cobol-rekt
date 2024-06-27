package org.smojol.ast;

import lombok.Getter;
import org.antlr.v4.runtime.tree.*;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;

import java.util.ArrayList;
import java.util.List;

public class NodeCollector implements ParseTreeVisitor<List<FlowNode>> {
    private final FlowNodeService nodeService;
    @Getter private final List<FlowNode> collectedNodes = new ArrayList<>();

    public NodeCollector(FlowNodeService nodeService) {
        this.nodeService = nodeService;
    }

    @Override
    public List<FlowNode> visit(ParseTree parseTree) {
        if (parseTree.getClass() == CobolParser.EndClauseContext.class || parseTree.getClass() == IdmsParser.EndClauseContext.class)
            return collectedNodes;
        FlowNode node = nodeService.existingNode(parseTree);
        if (node == null) return collectedNodes;
        collectedNodes.add(node);
        return collectedNodes;
    }

    @Override
    public List<FlowNode> visitChildren(RuleNode ruleNode) {
        if (ruleNode.getClass() == CobolParser.EndClauseContext.class || ruleNode.getClass() == IdmsParser.EndClauseContext.class)
            return collectedNodes;
        FlowNode node = nodeService.existingNode(ruleNode);
        if (node != null) collectedNodes.add(node);
        for (int i = 0; i < ruleNode.getChildCount(); i++) {
            ruleNode.getChild(i).accept(this);
        }
        return collectedNodes;
    }

    @Override
    public List<FlowNode> visitTerminal(TerminalNode terminalNode) {
        return collectedNodes;
    }

    @Override
    public List<FlowNode> visitErrorNode(ErrorNode errorNode) {
        return collectedNodes;
    }
}
