package org.smojol.common.navigation;

import org.smojol.common.ast.AggregatingFlowNodeASTVisitor;

import java.util.List;
import java.util.function.Function;
import java.util.logging.Logger;

public class AggregatingTreeTraversal<T, R> {
    private static final Logger LOGGER = Logger.getLogger(AggregatingFlowNodeASTVisitor.class.getName());

    public R accept(T node, AggregatingFlowNodeASTVisitor<T, R> visitor, Function<T, List<T>> childrenFn) {
        visitor.enter(node);
        visitor.visit(node);
        LOGGER.finer("Checking node : " + node.toString());
        List<R> childResults = childrenFn.apply(node).stream().map(c -> accept(c, visitor.scope(c), childrenFn)).toList();
        R result = visitor.processChildResults(node, childResults);
        visitor.exit(node);
        return result;
    }
}
