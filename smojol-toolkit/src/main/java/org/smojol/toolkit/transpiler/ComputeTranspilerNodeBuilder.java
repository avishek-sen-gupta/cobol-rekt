package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.SetTranspilerNode;
import org.smojol.common.transpiler.TranspilerCodeBlock;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.ComputeFlowNode;

public class ComputeTranspilerNodeBuilder {
    public static TranspilerNode build(ComputeFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode rhs = nodeBuilder.build(n.getRhsExpression());
        return new TranspilerCodeBlock(n.getDestinationExpressions().stream().map(dst -> (TranspilerNode) new SetTranspilerNode(rhs, nodeBuilder.build(dst))).toList());
    }
}
