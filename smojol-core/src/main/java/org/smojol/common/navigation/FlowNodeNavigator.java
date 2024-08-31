package org.smojol.common.navigation;

import com.google.common.collect.ImmutableList;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeCondition;

import java.util.ArrayList;
import java.util.List;

public class FlowNodeNavigator {
    private final FlowNode root;

    public FlowNodeNavigator(FlowNode root) {
        this.root = root;
    }

    public FlowNode findByCondition(FlowNodeCondition c) {
        return searchRecursively(root, c);
    }

    public <T extends FlowNode> T findByType(Class<T> type) {
        return (T) findByCondition(fn -> fn.getClass() == type);
    }

    private FlowNode searchRecursively(FlowNode current, FlowNodeCondition c) {
        if (c.apply(current)) return current;
        for (FlowNode child : current.astChildren()) {
            FlowNode found = searchRecursively(child, c);
            if (found != null) return found;
        }
        return null;
    }

    public List<FlowNode> findAllByCondition(FlowNodeCondition c) {
        return searchAllRecursively(root, c);
    }

    private List<FlowNode> searchAllRecursively(FlowNode current, FlowNodeCondition c) {
        if (c.apply(current)) return ImmutableList.of(current);
        ArrayList<FlowNode> flowNodes = new ArrayList<>();
        for (FlowNode child : current.astChildren()) {
            List<FlowNode> found = searchAllRecursively(child, c);
            flowNodes.addAll(found);
        }
        return flowNodes;
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
