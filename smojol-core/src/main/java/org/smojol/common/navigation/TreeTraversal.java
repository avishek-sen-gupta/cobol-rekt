package org.smojol.common.navigation;

import org.smojol.common.ast.NodeVisitor;

import java.util.logging.Logger;

public class TreeTraversal<T extends TreeNode<T>> {
    private static final Logger LOGGER = Logger.getLogger(TreeTraversal.class.getName());

    public void run(T node, NodeVisitor<T> visitor) {
        LOGGER.finer("Checking node : " + node);
        visitor.visit(node);
        NodeVisitor<T> scopedVisitor = visitor.scope(node);
        node.astChildren().forEach(c -> run(c, scopedVisitor));
    }
}
