package org.smojol.toolkit.transpiler;

import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.transpiler.IfTranspilerNode;
import org.smojol.common.transpiler.TranspilerCodeBlock;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.GenericOnClauseFlowNode;
import org.smojol.toolkit.ast.IfFlowNode;
import org.smojol.toolkit.ast.SearchFlowNode;

public class IfTranspilerNodeBuilder {
    public static TranspilerNode build(IfFlowNode n, CobolDataStructure dataStructures, CodeSentinelType codeSentinelType) {
        CobolExpression condition = n.getConditionExpression();
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode transpilerCondition = nodeBuilder.build(condition);
        TranspilerCodeBlock ifThenBlock = new TranspilerCodeBlock(n.getIfThenBlock().astChildren().stream().map(stmt -> TranspilerTreeBuilder.flowToTranspiler(stmt, dataStructures, codeSentinelType)).toList());
        TranspilerCodeBlock ifElseBlock = n.getIfElseBlock() != null ? new TranspilerCodeBlock(n.getIfElseBlock().astChildren().stream().map(stmt -> TranspilerTreeBuilder.flowToTranspiler(stmt, dataStructures, codeSentinelType)).toList()) : new TranspilerCodeBlock();
        return new IfTranspilerNode(transpilerCondition, ifThenBlock, ifElseBlock);
    }

    public static TranspilerNode build(GenericOnClauseFlowNode n, CobolDataStructure dataStructures) {
        return null;
    }

    public static TranspilerNode build(SearchFlowNode n, CobolDataStructure dataStructures) {
        return null;
    }
}
