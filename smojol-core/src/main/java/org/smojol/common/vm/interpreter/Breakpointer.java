package org.smojol.common.vm.interpreter;

import org.smojol.common.ast.FlowNodeCondition;

public interface Breakpointer extends ExecutionInterceptor {
    void addBreakpoint(FlowNodeCondition breakpoint);
}
