package org.smojol.common.vm.interpreter;

import java.util.function.Supplier;
import org.smojol.common.vm.stack.ExecutionContext;

public interface ExecutionInterceptor {
  CobolVmSignal run(Supplier<CobolVmSignal> execution, ExecutionContext executionContext);
}
