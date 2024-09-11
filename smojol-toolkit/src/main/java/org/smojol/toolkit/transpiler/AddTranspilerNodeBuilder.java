package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.AddFlowNode;

public class AddTranspilerNodeBuilder implements TranspilerNode {
    public static TranspilerNode build(AddFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode fromSum = n.getSourceExpressions().stream().map(nodeBuilder::build).reduce((subExpr, summand) -> new AddNode(summand, subExpr)).get();
        return new TranspilerCodeBlock(n.getDestinationExpressions().stream().map(to -> (TranspilerNode) new SetTranspilerNode(fromSum, nodeBuilder.build(to))).toList()).unwrap();
    }
}
