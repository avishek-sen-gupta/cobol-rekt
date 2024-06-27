package org.smojol.ast;

import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.*;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

public class GenericOnClauseFlowNode extends CompositeCobolFlowNode {
    @Getter private FlowNode condition;

    public GenericOnClauseFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.GenericOnClauseStatementContext onClause = new SyntaxIdentity<CobolParser.GenericOnClauseStatementContext>(executionContext).get();
        condition = nodeService.node(onClause.generalIdentifier(), this, staticFrameContext);
        super.buildInternalFlow();
    }

    @Override
    public List<? extends ParseTree> getChildren() {
        CobolParser.GenericOnClauseStatementContext onClause = new SyntaxIdentity<CobolParser.GenericOnClauseStatementContext>(executionContext).get();
        return onClause.statement();
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
        visitor.visitParentChildLink(this, internalTreeRoot, new VisitContext(level), nodeService, CHILD_IS_CONDITIONAL_STATEMENT);
    }
}
