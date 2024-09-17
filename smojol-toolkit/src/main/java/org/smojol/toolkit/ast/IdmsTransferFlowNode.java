package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.*;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class IdmsTransferFlowNode extends CobolFlowNode implements ExternalControlFlowNode {
    private CallTarget callTarget;

    public IdmsTransferFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames staticFrameContext) {
        super(parseTree, scope, nodeService, staticFrameContext);
    }

    @Override
    public void buildInternalFlow() {
        CobolEntityNavigator navigator = nodeService.getNavigator();
        IdmsParser.TransferStatementContext transfer = (IdmsParser.TransferStatementContext) navigator.findByCondition(getExecutionContext(), n -> n.getClass() == IdmsParser.TransferStatementContext.class);
        callTarget = CallTargetBuilder.target(transfer);
    }

    @Override
    public String name() {
        return NodeText.dialectOriginalText(getExecutionContext(), nodeService);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.TRANSFER;
    }

    @Override
    public CallTarget callTarget() {
        return callTarget;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.CONTROL_FLOW);
    }
}
