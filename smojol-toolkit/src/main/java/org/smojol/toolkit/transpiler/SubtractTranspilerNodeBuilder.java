package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.SubtractFlowNode;

import java.util.List;

import static com.google.common.collect.Streams.zip;

public class SubtractTranspilerNodeBuilder {
    public static TranspilerNode build(SubtractFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode subtrahendSum = n.getSubtrahendExpressions().stream().map(nodeBuilder::build).reduce((subExpr, summand) -> new AddNode(summand, subExpr)).get();
        List<SubtractNode> differences = n.getMinuendExpressions().stream().map(minuend -> new SubtractNode(nodeBuilder.build(minuend), subtrahendSum)).toList();

        if (!n.isGiving())
            return new TranspilerCodeBlock(zip(n.getDestinationExpressions().stream().map(nodeBuilder::build), differences.stream(), (dst, src) -> (TranspilerNode) new SetTranspilerNode(src, dst)).toList());
        return new TranspilerCodeBlock(n.getDestinationExpressions().stream().map(dst -> (TranspilerNode) new SetTranspilerNode(differences.getFirst(), nodeBuilder.build(dst))).toList());
    }
}
