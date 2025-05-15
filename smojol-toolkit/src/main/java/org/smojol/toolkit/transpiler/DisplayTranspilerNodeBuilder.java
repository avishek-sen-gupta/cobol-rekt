package org.smojol.toolkit.transpiler;

import com.mojo.algorithms.transpiler.PrintTranspilerNode;
import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.transpiler.ValueOfNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.DisplayFlowNode;

public class DisplayTranspilerNodeBuilder {
    public static TranspilerNode build(DisplayFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        return new PrintTranspilerNode(n.getOperandExpressions().stream().map(expression -> (TranspilerNode) new ValueOfNode(nodeBuilder.build(expression))).toList());
    }
}
