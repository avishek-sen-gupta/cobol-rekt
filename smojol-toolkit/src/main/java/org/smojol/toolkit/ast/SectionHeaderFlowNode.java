package org.smojol.toolkit.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import com.mojo.algorithms.transpiler.FlowNodeType;
import org.smojol.common.vm.stack.StackFrames;

public class SectionHeaderFlowNode extends CobolFlowNode {
    public SectionHeaderFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.SECTION_HEADER;
    }

    @Override
    public FlowNode passthrough() {
        return outgoingNodes.getFirst();
    }

    @Override
    public boolean isPassthrough() {
        return true;
    }
}
