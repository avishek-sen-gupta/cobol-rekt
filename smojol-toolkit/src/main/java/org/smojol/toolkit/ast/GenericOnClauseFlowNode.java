package org.smojol.toolkit.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.*;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

@Getter
public class GenericOnClauseFlowNode extends CobolFlowNode {
    private FlowNode condition;
    private OnClauseActionsFlowNode onClauseBlock;

    public GenericOnClauseFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.GenericOnClauseStatementContext onClause = new SyntaxIdentity<CobolParser.GenericOnClauseStatementContext>(executionContext).get();
        condition = nodeService.node(onClause.generalIdentifier(), this, staticFrameContext);
        onClauseBlock = new OnClauseActionsFlowNode(onClause.onClauseBlock(), this, nodeService, staticFrameContext);
        super.buildInternalFlow();
    }

    @Override
    public List<? extends ParseTree> getChildren() {
        CobolParser.GenericOnClauseStatementContext onClause = new SyntaxIdentity<CobolParser.GenericOnClauseStatementContext>(executionContext).get();
        return ImmutableList.of(onClause.onClauseBlock());
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.ON_CLAUSE;
    }

    @Override
    public String label() {
        return "ON\n" + NodeText.originalText(condition.getExecutionContext(), NodeText::PASSTHROUGH);
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executeOnClause(this, nodeService);
        return flowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), signal);
    }

    @Override
    public void linkParentToChild(FlowNodeVisitor visitor, int level) {
        visitor.visitParentChildLink(this, onClauseBlock, new VisitContext(level), nodeService);
    }

    @Override
    public List<SemanticCategory> categories() {
        return ImmutableList.of(SemanticCategory.DECISION);
    }

    @Override
    public List<FlowNode> astChildren() {
        return ImmutableList.of(onClauseBlock);
    }

    @Override
    public void resolve(SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        condition.resolve(symbolTable, dataStructures);
        onClauseBlock.resolve(symbolTable, dataStructures);
    }
}
