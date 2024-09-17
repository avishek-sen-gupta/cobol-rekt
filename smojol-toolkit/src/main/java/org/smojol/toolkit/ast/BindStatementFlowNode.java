package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.*;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class BindStatementFlowNode extends CobolFlowNode {
    public BindStatementFlowNode(IdmsParser.BindStatementContext parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public String label() {
        return originalText();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.BIND_RUN_UNIT;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.TRANSACTION_START, SemanticCategory.TRANSACTION);
    }
}
