package org.smojol.common.ast;

public abstract class FlowNodeASTVisitor<T> {
    protected final T ancestor;

    public FlowNodeASTVisitor(T ancestor) {
        this.ancestor = ancestor;
    }

    public abstract FlowNodeASTVisitor<T> visit(FlowNode node);
    public T root() {
        return ancestor;
    }
}
