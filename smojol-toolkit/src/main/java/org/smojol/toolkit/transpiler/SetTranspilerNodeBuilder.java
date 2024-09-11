package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.transpiler.ValueOfNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.MoveFlowNode;

import java.util.List;

public class SetTranspilerNodeBuilder implements TranspilerNode {
    public static TranspilerNode build(MoveFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        List<TranspilerNode> tos = n.getToExpressions().stream().map(nodeBuilder::build).toList();
        TranspilerNode from = new ValueOfNode(nodeBuilder.build(n.getFromExpression()));
        return new TranspilerCodeBlock(tos.stream().map(to -> (TranspilerNode) new SetTranspilerNode(from, to)).toList()).unwrap();
    }
}
