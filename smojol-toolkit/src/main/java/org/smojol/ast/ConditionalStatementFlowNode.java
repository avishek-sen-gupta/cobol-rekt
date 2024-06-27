package org.smojol.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.common.flowchart.FlowNodeType;
import org.smojol.common.vm.stack.StackFrames;

public class ConditionalStatementFlowNode extends CompositeCobolFlowNode {
    public ConditionalStatementFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.CONDITIONAL_STATEMENT;
    }

    @Override
    public boolean isMergeable() {
        return internalTreeRoot.isMergeable();
    }

    @Override
    public String name() {
        return executionContext.getText();
    }

    @Override
    public boolean contains(FlowNode node) {
        return internalTreeRoot == node;
    }
}
