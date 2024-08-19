package org.smojol.toolkit.ast;

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
public class ComputeFlowNode extends CobolFlowNode {
    private List<CobolParser.ComputeStoreContext> destinations;
    private CobolParser.ArithmeticExpressionContext rhs;

    public ComputeFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public void buildInternalFlow() {
        CobolParser.ComputeStatementContext computeStatement = new SyntaxIdentity<CobolParser.ComputeStatementContext>(executionContext).get();
        destinations = computeStatement.computeStore();
        rhs = computeStatement.arithmeticExpression();
        super.buildInternalFlow();
    }

    @Override
    public FlowNodeType type() {
        return FlowNodeType.COMPUTE;
    }

    @Override
    public CobolVmSignal acceptInterpreter(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = interpreter.scope(this).executeCompute(this, nodeService);
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
