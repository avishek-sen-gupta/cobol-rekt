package org.smojol.toolkit.transpiler;

import com.mojo.algorithms.transpiler.MultiplyNode;
import com.mojo.algorithms.transpiler.SetTranspilerNode;
import com.mojo.algorithms.transpiler.TranspilerCodeBlockNode;
import com.mojo.algorithms.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.MultiplyFlowNode;

public class MultiplyTranspilerNodeBuilder {
    public static TranspilerNode build(MultiplyFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode from = nodeBuilder.build(n.getSourceExpressions().getFirst());
        TranspilerNode product = n.getSourceExpressions().stream().map(nodeBuilder::build).reduce(MultiplyNode::new).get();
        if (!n.isGiving())
            return new TranspilerCodeBlockNode(n.getDestinationExpressions().stream().map(dst -> (TranspilerNode) new SetTranspilerNode(product, nodeBuilder.build(dst))).toList());
        return new TranspilerCodeBlockNode(n.getDestinationExpressions().stream().map(dst -> (TranspilerNode) new SetTranspilerNode(product, nodeBuilder.build(dst))).toList());
    }
}
