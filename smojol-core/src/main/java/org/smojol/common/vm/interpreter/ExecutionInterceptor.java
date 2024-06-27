package org.smojol.common.vm.interpreter;

import org.smojol.common.vm.stack.ExecutionContext;

import java.util.function.Supplier;

public interface ExecutionInterceptor {
    CobolVmSignal run(Supplier<CobolVmSignal> execution, ExecutionContext executionContext);
}
