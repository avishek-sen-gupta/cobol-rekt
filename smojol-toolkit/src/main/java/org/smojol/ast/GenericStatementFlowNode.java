package org.smojol.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeService;
import org.smojol.common.flowchart.FlowNodeType;
import org.smojol.common.vm.stack.StackFrames;

public class GenericStatementFlowNode extends CobolFlowNode {
    public GenericStatementFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public boolean isMergeable() {
        return true;
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.GENERIC_STATEMENT;
    }
}
