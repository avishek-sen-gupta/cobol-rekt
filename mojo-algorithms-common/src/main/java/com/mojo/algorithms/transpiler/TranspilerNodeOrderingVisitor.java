package com.mojo.algorithms.transpiler;


import com.mojo.algorithms.domain.JumpType;
import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.navigation.NodeVisitor;

import java.util.HashMap;
import java.util.Map;

public class TranspilerNodeOrderingVisitor implements NodeVisitor<TranspilerNode> {
    private int counter = 0;
    private final Map<TranspilerNode, Integer> order = new HashMap<>();

    @Override
    public NodeVisitor<TranspilerNode> scope(TranspilerNode n) {
        return this;
    }

    @Override
    public void visit(TranspilerNode n) {
        order.put(n, counter++);
    }

    public JumpType order(TranspilerNode jump, TranspilerNode destination) {
        return order.get(jump) > order.get(destination) ? JumpType.BACKWARD : JumpType.FORWARD;
    }
}
