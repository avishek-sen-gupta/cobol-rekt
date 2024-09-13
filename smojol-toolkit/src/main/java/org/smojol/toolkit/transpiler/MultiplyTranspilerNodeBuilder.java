package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.MultiplyFlowNode;

import java.util.List;

import static com.google.common.collect.Streams.zip;

public class MultiplyTranspilerNodeBuilder {
    public static TranspilerNode build(MultiplyFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode from = nodeBuilder.build(n.getSourceExpressions().getFirst());
        TranspilerNode product = n.getSourceExpressions().stream().map(nodeBuilder::build).reduce(MultiplyNode::new).get();
        if (!n.isGiving())
            return new TranspilerCodeBlock(n.getDestinationExpressions().stream().map(dst -> (TranspilerNode) new SetTranspilerNode(product, nodeBuilder.build(dst))).toList()).unwrap();
        return new TranspilerCodeBlock(n.getDestinationExpressions().stream().map(dst -> (TranspilerNode) new SetTranspilerNode(product, nodeBuilder.build(dst))).toList()).unwrap();
    }
}