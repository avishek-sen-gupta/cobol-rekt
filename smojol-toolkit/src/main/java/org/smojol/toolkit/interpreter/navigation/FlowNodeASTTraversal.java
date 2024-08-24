package org.smojol.toolkit.interpreter.navigation;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeASTVisitor;

public class FlowNodeASTTraversal<T> {
    public void accept(FlowNode node, FlowNodeASTVisitor<T> visitor) {
        T nodeResult = visitor.visit(node);
        node.astChildren().forEach(c -> accept(c, visitor.scope(c, nodeResult)));
    }
}
