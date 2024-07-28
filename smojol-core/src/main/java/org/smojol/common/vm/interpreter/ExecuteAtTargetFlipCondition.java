package org.smojol.common.vm.interpreter;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.vm.stack.ExecutionContext;

import java.util.function.Supplier;

/**
 * This is specifically used to skip over statements when navigating to a GO TO target
 * which is in the middle of a section, while still building the correct stack frames.
 * Nodes which are encountered before the GO TO target are simply not executed.
 * When the target is encountered, the internal switch is flipped to true, and stays true thereafter.
 */
public class ExecuteAtTargetFlipCondition implements ExecuteCondition {
    private final FlowNode specificLocation;
    private boolean isInterpreting = false;

    public ExecuteAtTargetFlipCondition(FlowNode specificLocation) {
        this.specificLocation = specificLocation;
    }

    @Override
    public boolean evaluate(FlowNode flowNode) {
        if (flowNode == specificLocation && !isInterpreting) {
            isInterpreting = true;
            System.out.println("FLIPPED TO TRUE");
        }
        return isInterpreting;
    }

    @Override
    public void run(Runnable e) {
        if (!isInterpreting) return;
        e.run();
    }

    @Override
    public CobolVmSignal run(Supplier<CobolVmSignal> execution, ExecutionContext executionContext) {
        if (!isInterpreting) return CobolVmSignal.CONTINUE;
        return execution.get();
    }
}
