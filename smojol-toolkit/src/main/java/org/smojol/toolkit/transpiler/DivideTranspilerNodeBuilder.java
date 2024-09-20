package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.DivideNode;
import org.smojol.common.transpiler.SetTranspilerNode;
import org.smojol.common.transpiler.TranspilerCodeBlock;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.DivideFlowNode;

import java.util.List;

import static com.google.common.collect.Streams.zip;

public class DivideTranspilerNodeBuilder {
    public static TranspilerNode build(DivideFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode divisor = nodeBuilder.build(n.getDivisorExpression());
        List<DivideNode> quotients = n.getDividendExpressions().stream().map(dividend -> new DivideNode(nodeBuilder.build(dividend), divisor)).toList();
        return setTranspilerNode(n, nodeBuilder, quotients);
    }

    private static TranspilerNode setTranspilerNode(DivideFlowNode n, TranspilerExpressionBuilder nodeBuilder, List<DivideNode> quotients) {
        if (!n.isGiving())
            return new TranspilerCodeBlock(zip(n.getDestinationExpressions().stream().map(nodeBuilder::build), quotients.stream(), (dst, src) -> (TranspilerNode) new SetTranspilerNode(src, dst)).toList());
        return new TranspilerCodeBlock(n.getDestinationExpressions().stream().map(dst -> (TranspilerNode) new SetTranspilerNode(quotients.getFirst(), nodeBuilder.build(dst))).toList());
    }
}
