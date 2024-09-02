package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.*;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class OnClauseActionFlowNode extends CompositeCobolFlowNode {
    @Override
    public String label() {
        return "No";
    }

    public OnClauseActionFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.ON_CLAUSE_ACTION;
    }

//    @Override
//    public String name() {
//        return "No";
//    }

//    @Override
//    public void linkParentToChild(FlowNodeVisitor visitor, int level) {
//        visitor.visitParentChildLink(this, internalTreeRoot, new VisitContext(level), nodeService, CHILD_IS_CONDITIONAL_STATEMENT);
//    }
//
    @Override
    public List<FlowNodeCategory> categories() {
        return ImmutableList.of(FlowNodeCategory.GENERIC_CODE);
    }
}