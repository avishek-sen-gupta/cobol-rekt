package org.smojol.common.navigation;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeCondition;

public class FlowNodeNavigator {
    private final FlowNode root;

    public FlowNodeNavigator(FlowNode root) {
        this.root = root;
    }

    public FlowNode findByCondition(FlowNodeCondition c) {
        return searchRecursively(root, c);
    }

    private FlowNode searchRecursively(FlowNode current, FlowNodeCondition c) {
        if (c.apply(current)) return current;
        for (FlowNode child : current.astChildren()) {
            FlowNode found = searchRecursively(child, c);
            if (found != null) return found;
        }
        return null;
    }

    public FlowNode findNarrowestByCondition(FlowNodeCondition c) {
        return searchNarrowestRecursively(root, c);
    }

    private FlowNode searchNarrowestRecursively(FlowNode current, FlowNodeCondition c) {
        if (!c.apply(current)) return null;
        for (FlowNode child : current.astChildren()) {
            FlowNode found = searchNarrowestRecursively(child, c);
            if (found != null) return found;
        }
        return current;
    }
}