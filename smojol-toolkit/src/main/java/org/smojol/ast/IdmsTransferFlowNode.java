package org.smojol.ast;

import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.*;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.stack.StackFrames;

public class IdmsTransferFlowNode extends CobolFlowNode implements ControlFlowNode {
    private CallTarget callTarget;

    public IdmsTransferFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames staticFrameContext) {
        super(parseTree, scope, nodeService, staticFrameContext);
    }

    @Override
    public void buildInternalFlow() {
        CobolEntityNavigator navigator = nodeService.getNavigator();
        IdmsParser.TransferStatementContext transfer = (IdmsParser.TransferStatementContext) navigator.findByCondition(getExecutionContext(), n -> n.getClass() == IdmsParser.TransferStatementContext.class);
        callTarget = CallTarget.target(transfer);
    }

    @Override
    public String name() {
        return NodeText.idmsOriginalText(getExecutionContext(), nodeService);
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.TRANSFER;
    }

    @Override
    public CallTarget callTarget() {
        return callTarget;
    }
}
