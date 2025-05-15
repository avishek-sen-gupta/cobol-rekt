package com.mojo.algorithms.navigation;


import java.util.List;
import java.util.function.Function;
import java.util.logging.Logger;

public class TreeMapperTraversal<T, R> {
    private static final Logger LOGGER = Logger.getLogger(TreeMapperVisitor.class.getName());

    public R accept(T node, TreeMapperVisitor<T, R> visitor, Function<T, List<T>> childrenFn) {
        visitor.enter(node);
        visitor.visit(node);
        LOGGER.finer("Checking node : " + node.toString());
        List<R> childResults = childrenFn.apply(node).stream().map(c -> accept(c, visitor.scope(c), childrenFn)).toList();
        R result = visitor.processChildResults(node, childResults);
        visitor.exit(node);
        return result;
    }
}
