package org.smojol.toolkit.transpiler;

import com.mojo.algorithms.transpiler.SetTranspilerNode;
import com.mojo.algorithms.transpiler.TranspilerCodeBlockNode;
import com.mojo.algorithms.domain.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.ComputeFlowNode;

public class ComputeTranspilerNodeBuilder {
    public static TranspilerNode build(ComputeFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode rhs = nodeBuilder.build(n.getRhsExpression());
        return new TranspilerCodeBlockNode(n.getDestinationExpressions().stream().map(dst -> (TranspilerNode) new SetTranspilerNode(rhs, nodeBuilder.build(dst))).toList());
    }
}
