package org.smojol.common.ast;

import org.smojol.common.navigation.GenericTreeNode;

public interface NodeVisitor<T extends GenericTreeNode> {
    NodeVisitor<T> scope(T n);
    void visit(T n);
}
