package org.smojol.interpreter.stack;

import org.smojol.ast.GoToFlowNode;
import org.smojol.ast.PerformProcedureFlowNode;
import org.smojol.common.flowchart.FlowNode;
import org.smojol.common.flowchart.FlowNodeCondition;
import org.smojol.common.vm.interpreter.CobolStackFrame;
import org.smojol.common.vm.interpreter.CobolVmSignal;
import org.smojol.common.vm.stack.IStackFrame;
import org.smojol.common.vm.stack.RootStackFrame;
import org.smojol.common.vm.stack.StackFrames;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.structure.NullDataStructure;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class CobolStackFrames implements StackFrames {
    private final CobolDataStructure dataStructures;
    List<IStackFrame> frames;

    // Only used for static stack frames
    public CobolStackFrames() {
        this(new NullDataStructure("ROOT"), new ArrayList<>());
    }

    // Used when adding a new frame
    @Deprecated public CobolStackFrames(List<IStackFrame> frames) {
        this(new NullDataStructure("ROOT"), frames);
    }

    // Used at VM startup
    public CobolStackFrames(CobolDataStructure dataStructures) {
        this(dataStructures, List.of(new RootStackFrame(dataStructures)));
    }

    private CobolStackFrames(CobolDataStructure dataStructures, List<IStackFrame> frames) {
        this.dataStructures = dataStructures;
        this.frames = frames;
    }

    @Override
    public StackFrames add(FlowNode frameContext) {
        List<IStackFrame> shallowCopy = new ArrayList<>(frames);
        // This will need to copy data structures later to allow framewise snapshots
        shallowCopy.add(new CobolStackFrame(frameContext, frames.isEmpty() ? dataStructures : frames.getLast().dataStructures()));
        return new CobolStackFrames(dataStructures, shallowCopy);
    }

    @Override
    public IStackFrame getLast() {
        return frames.getLast();
    }

    @Override
    public Optional<IStackFrame> find(FlowNodeCondition c) {
        return frames.stream().filter(f -> f.apply(c)).findFirst();
    }

    @Override
    public String stackTrace() {
        StringBuilder builder = new StringBuilder();
        frames.forEach(f -> builder.append("- ").append(f.description()).append("\n"));
        return builder.toString();
    }

    @Override
    public CobolVmSignal callSite() {
        int i = frames.size() - 1;
        while (i >= 0 && frames.get(i).isOfType(GoToFlowNode.class)) {
//            if (frames.get(i).getClass() == PerformProcedureChartNode.class) return CobolVmSignal.EXIT_PERFORM;
            if (frames.get(i).isOfType(PerformProcedureFlowNode.class)) return CobolVmSignal.EXIT_PERFORM;
            i --;
        }
        return CobolVmSignal.EXIT_SCOPE;
    }

    @Override
    public CobolDataStructure currentData() {
        return frames.getLast().dataStructures();
    }
}
