package org.smojol.common.ast;

public abstract class FlowNodeASTVisitor<T> {
    protected final T ancestor;


    public FlowNodeASTVisitor(T ancestor) {
        this.ancestor = ancestor;
    }

    public abstract T visit(FlowNode node);

    public T root() {
        return ancestor;
    }

    public abstract FlowNodeASTVisitor<T> scope(FlowNode n, T visitResult);
}
