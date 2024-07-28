package org.smojol.common.vm.stack;

import lombok.Getter;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeCondition;
import org.smojol.common.vm.structure.CobolDataStructure;

@Getter
public class RootStackFrame implements IStackFrame {
    private final CobolDataStructure dataStructures;

    public RootStackFrame(CobolDataStructure dataStructures) {
        this.dataStructures = dataStructures;
    }

    @Override
    public boolean apply(FlowNodeCondition c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String description() {
        return "[ROOT FRAME]";
    }

    @Override
    public boolean isOfType(Class<? extends FlowNode> chartNodeType) {
        return false;
    }

    @Override
    public FlowNode callSite() {
        throw new UnsupportedOperationException();
    }

    @Override
    public CobolDataStructure dataStructures() {
        return dataStructures;
    }
}
