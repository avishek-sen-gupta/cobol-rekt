package org.smojol.interpreter.interpreter;

import org.smojol.ast.*;
import org.smojol.common.vm.structure.CobolOperation;
import org.smojol.common.vm.structure.CobolOperations;
import org.smojol.common.vm.structure.NoOpCobolOperation;

public class RealOperations {
    public static CobolOperations OPS = new CobolOperations(
            node -> new MoveOperation((MoveFlowNode) node),
            node -> new ComputeOperation((ComputeFlowNode) node),
            node -> new AddOperation((AddFlowNode) node),
            node -> new SubtractOperation((SubtractFlowNode) node),
            node -> new MultiplyOperation((MultiplyFlowNode) node),
            node -> new DivideOperation((DivideFlowNode) node));
}
