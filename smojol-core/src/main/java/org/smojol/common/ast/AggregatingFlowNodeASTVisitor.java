package org.smojol.common.ast;

import java.util.List;

public abstract class AggregatingFlowNodeASTVisitor<T> {
    protected final FlowNode ancestor;

    public AggregatingFlowNodeASTVisitor(FlowNode ancestor) {
        this.ancestor = ancestor;
    }

    public abstract void visit(FlowNode node);
    public abstract void enter(FlowNode node);
    public abstract void exit(FlowNode node);
    public abstract AggregatingFlowNodeASTVisitor<T> scope(FlowNode n);
    public abstract void processChildResults(List<T> childResults);
    public abstract T result();
}
