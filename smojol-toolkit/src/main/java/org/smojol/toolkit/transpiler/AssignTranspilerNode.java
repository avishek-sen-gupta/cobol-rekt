package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.MoveFlowNode;

public class AssignTranspilerNode implements TranspilerNode {
    public AssignTranspilerNode(MoveFlowNode n, CobolDataStructure dataStructures) {
        CobolExpression first = n.getToExpressions().getFirst();
        TranspilerNodeBuilder nodeBuilder = new TranspilerNodeBuilder(dataStructures);
        TranspilerNode node = nodeBuilder.build(first);
    }
}
