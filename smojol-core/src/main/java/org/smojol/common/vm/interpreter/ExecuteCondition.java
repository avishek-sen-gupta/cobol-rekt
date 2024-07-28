package org.smojol.common.vm.interpreter;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.vm.stack.ExecutionContext;

import java.util.function.Supplier;

public interface ExecuteCondition extends ExecutionInterceptor {
    ExecuteCondition ALWAYS_EXECUTE = new ExecuteCondition() {

        @Override
        public boolean evaluate(FlowNode flowNode) {
            return true;
        }

        @Override
        public void run(Runnable e) {
            e.run();
        }

        @Override
        public CobolVmSignal run(Supplier<CobolVmSignal> execution, ExecutionContext executionContext) {
            return execution.get();
        }
    };

    boolean evaluate(FlowNode flowNode);
    void run(Runnable e);
}
