package org.smojol.common.vm.interpreter;

import java.util.function.Supplier;

/**
 * This interface specifically allows execution to be restricted to a single para/section, or not. This is so that
 * PERFORM can return after that single para/section.
 */
public interface FlowControl {
    static CobolVmSignal STOP(Supplier<CobolVmSignal> action, CobolVmSignal defaultSignal) {
        return defaultSignal;
    }

    static CobolVmSignal CONTINUE(Supplier<CobolVmSignal> action, CobolVmSignal defaultSignal) {
        return action.get();
    }

    CobolVmSignal apply(Supplier<CobolVmSignal> action, CobolVmSignal defaultSignal);
}
