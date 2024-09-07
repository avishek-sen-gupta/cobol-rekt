package org.smojol.toolkit.interpreter.navigation;

import org.smojol.common.ast.AggregatingFlowNodeASTVisitor;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeASTVisitor;

import java.util.List;
import java.util.logging.Logger;

public class AggregatingFlowNodeASTTraversal<T> {
    private static final Logger logger = Logger.getLogger(AggregatingFlowNodeASTVisitor.class.getName());

    public T accept(FlowNode node, AggregatingFlowNodeASTVisitor<T> visitor) {
        visitor.enter(node);
        visitor.visit(node);
        logger.finer("Checking node : " + node.label());
        List<T> childResults = node.astChildren().stream().map(c -> accept(c, visitor.scope(c))).toList();
        visitor.processChildResults(childResults);
        visitor.exit(node);
        return visitor.result();
    }
}
