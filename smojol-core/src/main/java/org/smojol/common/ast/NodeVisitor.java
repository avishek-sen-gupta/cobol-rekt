package org.smojol.common.ast;

import org.smojol.common.navigation.TreeNode;

public interface NodeVisitor<T extends TreeNode> {
    NodeVisitor<T> scope(T n);
    void visit(T n);
}
