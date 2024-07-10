package org.smojol.interpreter.interpreter;

import com.google.common.collect.ImmutableList;
import org.smojol.common.vm.interpreter.*;
import org.smojol.common.vm.structure.CobolOperations;
import org.smojol.interpreter.stack.CobolStackFrames;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

public class CobolInterpreterFactory {
    public static CobolInterpreter oldInterpreter(ConditionResolver conditionResolver, CobolDataStructure dataStructures, List<ExecutionInterceptor> interceptors, ExecutionListener listeners, Breakpointer bp) {
        return oldInterpreter(ExecuteCondition.ALWAYS_EXECUTE, conditionResolver, interceptors, listeners, bp, new CobolStackFrames(dataStructures));
    }

    public static CobolInterpreter oldInterpreter(ExecuteCondition condition, ConditionResolver conditionResolver, List<ExecutionInterceptor> interceptors, ExecutionListener listeners, Breakpointer bp, StackFrames runtimeStackFrames) {
        return new SmojolInterpreter(runtimeStackFrames, condition, conditionResolver, bp, interceptors, listeners);
    }

    public static CobolInterpreter interpreter(StackFrames stackFrames, ExecuteCondition condition, ConditionResolver conditionResolver, List<ExecutionInterceptor> interceptors, ExecutionListener listeners, Breakpointer bp, InterpreterBuilder interpreterBuilder, CobolOperations ops) {
        return new NonExecutingInterpreter(stackFrames, condition, conditionResolver, bp, interceptors, listeners, interpreterBuilder, ops);
    }

    public static CobolInterpreter interpreter(CobolDataStructure dataStructures, ExecuteCondition condition, ConditionResolver conditionResolver, ImmutableList<ExecutionInterceptor> interceptors, ExecutionListener listener, Breakpointer bp, InterpreterBuilder interpreterBuilder, CobolOperations ops) {
        return interpreter(new CobolStackFrames(dataStructures), condition, conditionResolver, interceptors, listener, bp, interpreterBuilder, ops);
    }

    public static CobolInterpreter executingInterpreter(CobolDataStructure dataStructures, ExecuteCondition condition, ConditionResolver conditionResolver, ImmutableList<ExecutionInterceptor> interceptors, ExecutionListener listener, Breakpointer bp, InterpreterBuilder interpreterBuilder) {
        return interpreter(new CobolStackFrames(dataStructures), condition, conditionResolver, interceptors, listener, bp, interpreterBuilder, RealOperations.OPS);
    }

    public static CobolInterpreter nonExecutingInterpreter(CobolDataStructure dataStructures, ExecuteCondition condition, ConditionResolver conditionResolver, ImmutableList<ExecutionInterceptor> interceptors, ExecutionListener listener, Breakpointer bp, InterpreterBuilder interpreterBuilder) {
        return interpreter(new CobolStackFrames(dataStructures), condition, conditionResolver, interceptors, listener, bp, interpreterBuilder, CobolOperations.NO_OP);
    }

    public static CobolInterpreter visitingInterpreter(CobolInterpreter interpreter, List<ExecutionInterceptor> interceptors, ExecutionListener listeners) {
        return new VisitingInterpreter(interpreter, interceptors, listeners);
    }
}
