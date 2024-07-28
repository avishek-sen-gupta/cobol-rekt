package org.smojol.common.ast;


public interface FlowNodeASTVisitor<T> {
    T visit(FlowNode node, T parent);
    T root();
}
