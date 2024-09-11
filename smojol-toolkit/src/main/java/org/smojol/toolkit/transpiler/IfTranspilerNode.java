package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.IfFlowNode;

public class IfTranspilerNode implements TranspilerNode {
    public IfTranspilerNode(IfFlowNode n, CobolDataStructure dataStructures) {
        CobolExpression condition = n.getConditionExpression();
        TranspilerNodeBuilder nodeBuilder = new TranspilerNodeBuilder(dataStructures);
        TranspilerNode node = nodeBuilder.build(condition);
    }
}
