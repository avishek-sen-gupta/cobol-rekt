package org.smojol.common.flowchart;


public interface FlowNodeASTVisitor<T> {
    void visit(FlowNode node, T parent, FlowNodeService nodeService);
}
