package org.smojol.common.vm.interpreter;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeCondition;
import org.smojol.common.ast.NodeText;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.stack.IStackFrame;

public record CobolStackFrame(FlowNode callSite, CobolDataStructure dataStructures) implements IStackFrame {
    @Override
    public boolean apply(FlowNodeCondition c) {
        return c.apply(callSite);
    }

    @Override
    public String description() {
        return callSite.getClass().getSimpleName() + "/" + NodeText.formatted(callSite.label());
    }

    @Override
    public boolean isOfType(Class<? extends FlowNode> chartNodeType) {
        return chartNodeType == callSite.getClass();
    }
}
