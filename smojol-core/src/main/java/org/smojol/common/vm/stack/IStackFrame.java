package org.smojol.common.vm.stack;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeCondition;
import org.smojol.common.vm.structure.CobolDataStructure;

public interface IStackFrame {
    boolean apply(FlowNodeCondition c);
    String description();
    boolean isOfType(Class<? extends FlowNode> chartNodeType);
    FlowNode callSite();
    CobolDataStructure dataStructures();
}
