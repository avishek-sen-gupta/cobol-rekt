package org.smojol.toolkit.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import com.mojo.algorithms.domain.FlowNodeType;
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
