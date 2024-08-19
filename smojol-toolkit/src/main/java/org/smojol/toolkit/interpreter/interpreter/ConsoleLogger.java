package org.smojol.toolkit.interpreter.interpreter;

import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.stack.ExecutionContext;
import org.smojol.common.vm.interpreter.ExecutionInterceptor;

import java.util.function.Supplier;

public class ConsoleLogger implements ExecutionInterceptor {
    @Override
    public CobolVmSignal run(Supplier<CobolVmSignal> execution, ExecutionContext executionContext) {
        CobolVmSignal signal = execution.get();
        System.out.println(ConsoleColors.coloured("After execution", 0, 155));
        return signal;
    }
}
