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
public class MultiplyFlowNode extends CobolFlowNode {
    private CobolParser.MultiplyLhsContext lhs;
    private List<CobolParser.MultiplyRegularOperandContext> rhs;
    private CobolParser.MultiplyGivingOperandContext givingRhs;
    private List<CobolParser.MultiplyGivingResultContext> givingDestinations;

    public MultiplyFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.MultiplyStatementContext multiplyStatement = new SyntaxIdentity<CobolParser.MultiplyStatementContext>(executionContext).get();
        lhs = multiplyStatement.multiplyLhs();
        if (multiplyStatement.multiplyRegular() != null) {
            rhs = multiplyStatement.multiplyRegular().multiplyRegularOperand();
            givingRhs = null;
            givingDestinations = ImmutableList.of();
        } else if (multiplyStatement.multiplyGiving() != null) {
            rhs = ImmutableList.of();
            givingRhs = multiplyStatement.multiplyGiving().multiplyGivingOperand();
            givingDestinations = multiplyStatement.multiplyGiving().multiplyGivingResult();
        }
        super.buildInternalFlow();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.MULTIPLY;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executeMultiply(this, nodeService);
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
