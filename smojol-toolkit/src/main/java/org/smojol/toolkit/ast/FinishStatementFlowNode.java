package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.dialects.idms.IdmsParser;
import org.smojol.common.ast.FlowNode;
import com.mojo.algorithms.transpiler.SemanticCategory;
import org.smojol.common.ast.FlowNodeService;
import com.mojo.algorithms.transpiler.FlowNodeType;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class FinishStatementFlowNode extends CobolFlowNode {
    public FinishStatementFlowNode(IdmsParser.FinishStatementContext parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public String label() {
        return originalText();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.FINISH;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.TRANSACTION_END, SemanticCategory.TRANSACTION);
    }
}
