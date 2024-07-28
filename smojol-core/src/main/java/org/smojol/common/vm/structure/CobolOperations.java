package org.smojol.common.vm.structure;

import org.smojol.common.ast.FlowNode;

import java.util.function.Function;

public record CobolOperations(Function<FlowNode, CobolOperation> move,
                              Function<FlowNode, CobolOperation> compute,
                              Function<FlowNode, CobolOperation> add,
                              Function<FlowNode, CobolOperation> subtract,
                              Function<FlowNode, CobolOperation> multiply,
                              Function<FlowNode, CobolOperation> divide) {
    public static CobolOperations NO_OP = new CobolOperations(
            NoOpCobolOperation::build,
            NoOpCobolOperation::build,
            NoOpCobolOperation::build,
            NoOpCobolOperation::build,
            NoOpCobolOperation::build,
            NoOpCobolOperation::build);
}
