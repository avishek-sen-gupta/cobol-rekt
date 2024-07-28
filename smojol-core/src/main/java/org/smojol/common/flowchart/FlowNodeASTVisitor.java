package org.smojol.common.flowchart;


public interface FlowNodeASTVisitor<T> {
    T visit(FlowNode node, T parent);
    T root();
}
