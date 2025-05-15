package com.mojo.algorithms.transpiler;


import com.mojo.algorithms.navigation.GenericTreeNode;

public interface NodeVisitor<T extends GenericTreeNode> {
    NodeVisitor<T> scope(T n);
    void visit(T n);
}
