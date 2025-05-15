package com.mojo.algorithms.navigation;


import com.mojo.algorithms.domain.TranspilerNode;

import java.util.List;
import java.util.logging.Logger;

public class AggregatingTranspilerNodeTraversal<T> {
    private static final Logger LOGGER = Logger.getLogger(AggregatingTranspilerNodeTraversal.class.getName());

    public T accept(TranspilerNode node, AggregatingTranspilerNodeVisitor<T> visitor) {
        visitor.enter(node);
        visitor.visit(node);
        LOGGER.finer("Checking node : " + node.description());
        List<T> childResults = node.astChildren().stream().map(c -> accept(c, visitor.scope(c))).toList();
        visitor.processChildResults(childResults);
        visitor.exit(node);
        return visitor.result();
    }
}
