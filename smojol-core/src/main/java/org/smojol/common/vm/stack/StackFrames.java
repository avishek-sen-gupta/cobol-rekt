package org.smojol.common.vm.stack;

import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeCondition;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.Optional;

public interface StackFrames {
    StackFrames add(FlowNode frame);
    IStackFrame getLast();
    Optional<IStackFrame> find(FlowNodeCondition c);
    String stackTrace();
    CobolVmSignal callSite();
    CobolDataStructure currentData();
}
