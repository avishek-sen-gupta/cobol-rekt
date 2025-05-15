package com.mojo.algorithms.navigation;


public interface NodeVisitor<T extends GenericTreeNode> {
    NodeVisitor<T> scope(T n);
    void visit(T n);
}
