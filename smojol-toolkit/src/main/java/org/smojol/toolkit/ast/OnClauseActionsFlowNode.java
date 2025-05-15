package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.FlowNodeType;
import com.mojo.algorithms.domain.SemanticCategory;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.*;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class OnClauseActionsFlowNode extends CompositeCobolFlowNode {
    @Override
    public String label() {
        return "No";
    }

    public OnClauseActionsFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.ON_CLAUSE_ACTION;
    }

//    @Override
//    public String adjacentSystem() {
//        return "No";
//    }

//    @Override
//    public void linkParentToChild(FlowNodeVisitor visitor, int level) {
//        visitor.visitParentChildLink(this, internalTreeRoot, new VisitContext(level), nodeService, CHILD_IS_CONDITIONAL_STATEMENT);
//    }
//
    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.GENERIC_CODE);
    }
}
