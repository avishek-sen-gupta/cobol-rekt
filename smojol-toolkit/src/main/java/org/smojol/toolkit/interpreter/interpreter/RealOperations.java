package org.smojol.toolkit.interpreter.interpreter;

import org.smojol.common.vm.structure.CobolOperations;
import org.smojol.toolkit.ast.*;

public class RealOperations {
    public static CobolOperations OPS = new CobolOperations(
            node -> new MoveOperation((MoveFlowNode) node),
            node -> new ComputeOperation((ComputeFlowNode) node),
            node -> new AddOperation((AddFlowNode) node),
            node -> new SubtractOperation((SubtractFlowNode) node),
            node -> new MultiplyOperation((MultiplyFlowNode) node),
            node -> new DivideOperation((DivideFlowNode) node));
}
