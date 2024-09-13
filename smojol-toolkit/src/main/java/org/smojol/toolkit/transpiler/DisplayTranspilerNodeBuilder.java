package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.PrintTranspilerNode;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.transpiler.ValueOfNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.DisplayFlowNode;

public class DisplayTranspilerNodeBuilder {
    public static TranspilerNode build(DisplayFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        return new PrintTranspilerNode(n.getOperandExpressions().stream().map(expression -> (TranspilerNode) new ValueOfNode(nodeBuilder.build(expression))).toList());
    }
}
