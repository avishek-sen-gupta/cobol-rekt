package org.smojol.toolkit.transpiler;

import com.mojo.algorithms.transpiler.SetTranspilerNode;
import com.mojo.algorithms.transpiler.TranspilerCodeBlockNode;
import com.mojo.algorithms.transpiler.TranspilerNode;
import com.mojo.algorithms.transpiler.ValueOfNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.MoveFlowNode;

import java.util.List;

public class SetTranspilerNodeBuilder {
    public static TranspilerNode build(MoveFlowNode n, CobolDataStructure dataStructures) {
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        List<TranspilerNode> tos = n.getToExpressions().stream().map(nodeBuilder::build).toList();
        TranspilerNode from = new ValueOfNode(nodeBuilder.build(n.getFromExpressions().getFirst()));
        return new TranspilerCodeBlockNode(tos.stream().map(to -> (TranspilerNode) new SetTranspilerNode(from, to)).toList());
    }
}
