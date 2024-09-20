package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.AddNode;
import org.smojol.common.transpiler.SetTranspilerNode;
import org.smojol.common.transpiler.TranspilerCodeBlock;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.AddFlowNode;

public class AddTranspilerNodeBuilder {
    public static TranspilerNode build(AddFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode fromSum = n.getSourceExpressions().stream().map(nodeBuilder::build).reduce(AddNode::new).get();
        return new TranspilerCodeBlock(n.getDestinationExpressions().stream().map(to -> (TranspilerNode) new SetTranspilerNode(fromSum, nodeBuilder.build(to))).toList());

//        if (!n.isGiving())
//            return new TranspilerCodeBlock(zip(n.getDestinationExpressions().stream().map(nodeBuilder::build), differences.stream(), (dst, src) -> (TranspilerNode) new SetTranspilerNode(src, dst)).toList()).unwrap();
//        return new TranspilerCodeBlock(n.getDestinationExpressions().stream().map(dst -> (TranspilerNode) new SetTranspilerNode(differences.getFirst(), nodeBuilder.build(dst))).toList()).unwrap();
    }
}
