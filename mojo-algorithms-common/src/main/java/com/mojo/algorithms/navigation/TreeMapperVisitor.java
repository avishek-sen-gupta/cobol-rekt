package com.mojo.algorithms.navigation;

import java.util.List;

public abstract class TreeMapperVisitor<T, R> {
    protected final T ancestor;

    public TreeMapperVisitor(T ancestor) {
        this.ancestor = ancestor;
    }

    public abstract void visit(T node);
    public abstract void enter(T node);
    public abstract void exit(T node);
    public abstract TreeMapperVisitor<T, R> scope(T n);
    public abstract R processChildResults(T node, List<R> mappedChildren);
}
