package org.smojol.toolkit.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.toolkit.interpreter.stack.CobolStackFrames;

public class BuildFlowNodesTask {
    private final FlowNodeService nodeService;

    public BuildFlowNodesTask(FlowNodeService nodeService) {
        this.nodeService = nodeService;
    }
    private FlowNode buildFlowAST(ParseTree node) {
        FlowNode rootFlowNode = nodeService.node(node, null, new CobolStackFrames());
        rootFlowNode.buildFlow();
        return rootFlowNode;
    }

    private FlowNode buildControlFlow(FlowNode root) {
        root.accept(new ControlFlowVisitor(), 1);
        return root;
    }

    public FlowNode run(ParseTree node) {
        return buildControlFlow(buildFlowAST(node));
    }
}
