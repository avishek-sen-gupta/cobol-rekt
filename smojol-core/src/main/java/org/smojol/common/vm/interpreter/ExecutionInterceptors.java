package org.smojol.common.vm.interpreter;

import org.smojol.common.vm.stack.ExecutionContext;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.function.Supplier;

public class ExecutionInterceptors implements ExecutionInterceptor {
    List<ExecutionInterceptor> interceptors;

    public ExecutionInterceptors(List<ExecutionInterceptor> interceptors) {
        this.interceptors = new ArrayList<>(interceptors);
    }

    @Override
    public CobolVmSignal run(Supplier<CobolVmSignal> execution, ExecutionContext executionContext) {
        return runRecursively(interceptors.listIterator(), execution, executionContext).get();
    }

    private Supplier<CobolVmSignal> runRecursively(ListIterator<ExecutionInterceptor> iterator, Supplier<CobolVmSignal> execution, ExecutionContext executionContext) {
        if (!iterator.hasNext()) return execution;
        ExecutionInterceptor interceptor = iterator.next();
        return () -> interceptor.run(runRecursively(iterator, execution, executionContext), executionContext);
    }

    public void addAll(List<ExecutionInterceptor> otherInterceptors) {
        interceptors.addAll(otherInterceptors);
    }
}
