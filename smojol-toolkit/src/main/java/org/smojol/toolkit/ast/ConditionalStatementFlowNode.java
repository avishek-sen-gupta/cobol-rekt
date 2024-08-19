package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeCategory;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

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

    @Override
    public List<FlowNodeCategory> categories() {
        return ImmutableList.of(FlowNodeCategory.CODE_BLOCK);
    }
}
