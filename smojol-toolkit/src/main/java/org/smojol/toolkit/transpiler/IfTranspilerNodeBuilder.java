package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.IfTranspilerNode;
import org.smojol.common.transpiler.TranspilerCodeBlock;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.IfFlowNode;

public class IfTranspilerNodeBuilder {
    public static TranspilerNode build(IfFlowNode n, CobolDataStructure dataStructures) {
        CobolExpression condition = n.getConditionExpression();
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode transpilerCondition = nodeBuilder.build(condition);
        TranspilerCodeBlock ifThenBlock = new TranspilerCodeBlock(n.getIfThenBlock().astChildren().stream().map(stmt -> TranspilerTreeBuilder.flowToTranspiler(stmt, dataStructures)).toList());
        TranspilerCodeBlock ifElseBlock = n.getIfElseBlock() != null ? new TranspilerCodeBlock(n.getIfElseBlock().astChildren().stream().map(stmt -> TranspilerTreeBuilder.flowToTranspiler(stmt, dataStructures)).toList()) : new TranspilerCodeBlock();
        return new IfTranspilerNode(transpilerCondition, ifThenBlock, ifElseBlock);
    }
}
