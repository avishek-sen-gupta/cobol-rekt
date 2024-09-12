package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.AddFlowNode;
import org.smojol.toolkit.ast.DivideFlowNode;

import java.util.List;

import static com.google.common.collect.Streams.zip;

public class AddTranspilerNodeBuilder {
    public static TranspilerNode build(AddFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode fromSum = n.getSourceExpressions().stream().map(nodeBuilder::build).reduce(AddNode::new).get();
        return new TranspilerCodeBlock(n.getDestinationExpressions().stream().map(to -> (TranspilerNode) new SetTranspilerNode(fromSum, nodeBuilder.build(to))).toList()).unwrap();

//        if (!n.isGiving())
//            return new TranspilerCodeBlock(zip(n.getDestinationExpressions().stream().map(nodeBuilder::build), differences.stream(), (dst, src) -> (TranspilerNode) new SetTranspilerNode(src, dst)).toList()).unwrap();
//        return new TranspilerCodeBlock(n.getDestinationExpressions().stream().map(dst -> (TranspilerNode) new SetTranspilerNode(differences.getFirst(), nodeBuilder.build(dst))).toList()).unwrap();
    }
}
