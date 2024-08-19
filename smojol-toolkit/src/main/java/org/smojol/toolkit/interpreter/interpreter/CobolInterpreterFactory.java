package org.smojol.toolkit.interpreter.interpreter;

import org.smojol.common.vm.interpreter.*;
import org.smojol.common.vm.structure.CobolOperations;
import org.smojol.toolkit.interpreter.stack.CobolStackFrames;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

public class CobolInterpreterFactory {
    public static CobolInterpreter executingInterpreter(ConditionResolver conditionResolver, CobolDataStructure dataStructures, List<ExecutionInterceptor> interceptors, ExecutionListener listeners, Breakpointer bp) {
        return interpreter(ExecuteCondition.ALWAYS_EXECUTE, conditionResolver, interceptors, listeners, bp, new CobolStackFrames(dataStructures), RealOperations.OPS);
    }

    public static CobolInterpreter nonExecutingInterpreter(ConditionResolver conditionResolver, CobolDataStructure dataStructures, List<ExecutionInterceptor> interceptors, ExecutionListener listeners, Breakpointer bp) {
        return interpreter(ExecuteCondition.ALWAYS_EXECUTE, conditionResolver, interceptors, listeners, bp, new CobolStackFrames(dataStructures), CobolOperations.NO_OP);
    }

    public static CobolInterpreter interpreter(ConditionResolver conditionResolver, CobolDataStructure dataStructures, List<ExecutionInterceptor> interceptors, ExecutionListener listeners, Breakpointer bp, CobolOperations operations) {
        return interpreter(ExecuteCondition.ALWAYS_EXECUTE, conditionResolver, interceptors, listeners, bp, new CobolStackFrames(dataStructures), operations);
    }

    public static CobolInterpreter interpreter(ExecuteCondition condition, ConditionResolver conditionResolver, List<ExecutionInterceptor> interceptors, ExecutionListener listeners, Breakpointer bp, StackFrames runtimeStackFrames, CobolOperations operations) {
        return new SmojolInterpreter(runtimeStackFrames, condition, conditionResolver, bp, interceptors, listeners, operations);
    }
}
