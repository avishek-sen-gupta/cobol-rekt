package org.smojol.ast;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.*;
import org.smojol.common.vm.interpreter.CobolInterpreter;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.stack.StackFrames;

public class ProcedureDivisionBodyFlowNode extends CompositeCobolFlowNode {
    @Override
    public FlowNodeType type() {
        return FlowNodeType.PROCEDURE_DIVISION_BODY;
    }

    public ProcedureDivisionBodyFlowNode(ParseTree parseTree, FlowNode scope, FlowNodeService nodeService, StackFrames stackFrames) {
        super(parseTree, scope, nodeService, stackFrames);
    }

    @Override
    public FlowNode next(FlowNodeCondition nodeCondition, FlowNode startingNode, boolean isComplete) {
        return new DummyFlowNode(nodeService, staticFrameContext);
    }

    @Override
    public CobolVmSignal acceptInterpreterForCompositeExecution(CobolInterpreter interpreter, FlowControl flowControl) {
        CobolVmSignal signal = super.acceptInterpreterForCompositeExecution(interpreter, flowControl);
        interpreter.signalTermination();
        System.out.println(ConsoleColors.red("Program has exited"));
        return signal;
    }
}
