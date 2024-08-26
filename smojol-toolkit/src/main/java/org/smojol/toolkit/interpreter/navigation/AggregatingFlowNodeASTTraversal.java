package org.smojol.toolkit.interpreter.navigation;

import org.smojol.common.ast.AggregatingFlowNodeASTVisitor;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeASTVisitor;

import java.util.List;

public class AggregatingFlowNodeASTTraversal<T> {
    public T accept(FlowNode node, AggregatingFlowNodeASTVisitor<T> visitor) {
        visitor.enter(node);
        visitor.visit(node);
        System.out.println("Checking node : " + node.label());
        List<T> childResults = node.astChildren().stream().map(c -> accept(c, visitor.scope(c))).toList();
        visitor.processChildResults(childResults);
        visitor.exit(node);
        return visitor.result();
    }
}
