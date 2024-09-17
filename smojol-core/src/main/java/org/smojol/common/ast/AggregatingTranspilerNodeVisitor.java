package org.smojol.common.ast;

import org.smojol.common.transpiler.TranspilerNode;

import java.util.List;

public abstract class AggregatingTranspilerNodeVisitor<T> {
    public abstract void visit(TranspilerNode node);
    public abstract void enter(TranspilerNode node);
    public abstract void exit(TranspilerNode node);
    public abstract AggregatingTranspilerNodeVisitor<T> scope(TranspilerNode n);
    public abstract void processChildResults(List<T> childResults);
    public abstract T result();
}
