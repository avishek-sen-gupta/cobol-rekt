package org.smojol.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;
import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.*;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;

import java.util.List;

@Getter
public class SubtractFlowNode extends CobolFlowNode {
    private List<CobolParser.SubtractSubtrahendContext> rhs;
    private List<CobolParser.SubtractMinuendContext> lhs;
    private CobolParser.SubtractMinuendGivingContext lhsGiving;
    private List<CobolParser.SubtractSubtrahendContext> rhsGiving;
    private List<CobolParser.SubtractGivingContext> givingDestinations;

    public SubtractFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.SubtractStatementContext subtractStatement = new SyntaxIdentity<CobolParser.SubtractStatementContext>(executionContext).get();
        if (subtractStatement.subtractFromStatement() != null) {
            lhs = subtractStatement.subtractFromStatement().subtractMinuend();
            rhs = subtractStatement.subtractFromStatement().subtractSubtrahend();
            lhsGiving = null;
            rhsGiving = ImmutableList.of();
            givingDestinations = ImmutableList.of();
        }
        else if (subtractStatement.subtractFromGivingStatement() != null) {
            lhs = ImmutableList.of();
            rhs = ImmutableList.of();
            lhsGiving = subtractStatement.subtractFromGivingStatement().subtractMinuendGiving();
            rhsGiving = subtractStatement.subtractFromGivingStatement().subtractSubtrahend();
            givingDestinations = subtractStatement.subtractFromGivingStatement().subtractGiving();
        }
        super.buildInternalFlow();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.SUBTRACT;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executeSubtract(this, nodeService);
        return flowControl.apply(() -> continueOrAbort(signal, interpreter, nodeService), signal);
    }

    @Override
    public boolean isMergeable() {
        return true;
    }

    @Override
    public List<FlowNodeCategory> categories() {
        return ImmutableList.of(FlowNodeCategory.COMPUTATIONAL, FlowNodeCategory.DATA_FLOW);
    }
}
