package org.smojol.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.*;
import org.smojol.common.vm.stack.StackFrames;

public class IfElseFlowNode extends CompositeCobolFlowNode {
    @Override
    public String label() {
        return "No";
    }

    public IfElseFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.IF_NO;
    }

    @Override
    public String name() {
        return "No";
    }

    @Override
    public void linkParentToChild(FlowNodeVisitor visitor, int level) {
        visitor.visitParentChildLink(this, internalTreeRoot, new VisitContext(level), nodeService, CHILD_IS_CONDITIONAL_STATEMENT);
    }
}
