package org.smojol.common.ast;

import java.util.List;

public abstract class AggregatingFlowNodeASTVisitor<T, R> {
    protected final T ancestor;

    public AggregatingFlowNodeASTVisitor(T ancestor) {
        this.ancestor = ancestor;
    }

    public abstract void visit(T node);
    public abstract void enter(T node);
    public abstract void exit(T node);
    public abstract AggregatingFlowNodeASTVisitor<T, R> scope(T n);
    public abstract R processChildResults(T node, List<R> childResults);
}
