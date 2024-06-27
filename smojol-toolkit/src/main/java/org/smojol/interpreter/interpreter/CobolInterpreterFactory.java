package org.smojol.interpreter.interpreter;

import org.smojol.common.vm.interpreter.*;
import org.smojol.interpreter.stack.CobolStackFrames;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

public class CobolInterpreterFactory {
//    public static CobolInterpreter interpreter() {
//        return interpreter(CobolConditionResolver.CONSOLE_RESOLVER, new CobolBreakpointer(), ImmutableList.of(), new ExecutionListeners(ImmutableList.of()));
//    }

    public static CobolInterpreter interpreter(ConditionResolver conditionResolver, CobolDataStructure dataStructures, List<ExecutionInterceptor> interceptors, ExecutionListener listeners, Breakpointer bp) {
        return interpreter(ExecuteCondition.ALWAYS_EXECUTE, conditionResolver, interceptors, listeners, bp, new CobolStackFrames(dataStructures));
    }

    public static CobolInterpreter interpreter(ExecuteCondition condition, ConditionResolver conditionResolver, List<ExecutionInterceptor> interceptors, ExecutionListener listeners, Breakpointer bp, StackFrames runtimeStackFrames) {
        return new SmojolInterpreter(runtimeStackFrames, condition, conditionResolver, bp, interceptors, listeners);
    }
}
