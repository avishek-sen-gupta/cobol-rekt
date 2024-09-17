package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.SemanticCategory;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.vm.stack.StackFrames;

import java.util.ArrayList;
import java.util.List;

public class AtEndPhraseFlowNode extends CompositeCobolFlowNode {
    private List<FlowNode> conditionalStatements = new ArrayList<>();
    public AtEndPhraseFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public List<? extends ParseTree> getChildren() {
        CobolParser.AtEndPhraseContext atEndPhrase = (CobolParser.AtEndPhraseContext) executionContext;
        return atEndPhrase.conditionalStatementCall();
    }

    @Override
    public String label() {
        return "";
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.AT_END_PHRASE;
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.CODE_BLOCK);
    }
}
