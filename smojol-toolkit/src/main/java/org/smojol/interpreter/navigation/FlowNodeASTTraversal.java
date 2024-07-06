package org.smojol.interpreter.navigation;

import org.smojol.common.flowchart.*;

import java.util.function.BiFunction;
import java.util.function.Function;

public class FlowNodeASTTraversal<T> {
    public T build(FlowNode tree, BiFunction<FlowNode, T, T> make, Function<FlowNode, Boolean> stopRecurseCondition) {
        return internalBuild(tree, null, make, stopRecurseCondition);
    }

    public T build(FlowNode tree, BiFunction<FlowNode, T, T> make) {
        return build(tree, make, n -> false);
    }

    public T internalBuild(FlowNode tree, T parent, BiFunction<FlowNode, T, T> make, Function<FlowNode, Boolean> stopRecurseCondition) {
        T node = make.apply(tree, parent);
        if (stopRecurseCondition.apply(tree)) return node;
        tree.astChildren().forEach(c -> internalBuild(c, node, make, stopRecurseCondition));
        return node;
    }
}