package org.smojol.toolkit.transpiler;

import com.mojo.algorithms.transpiler.DetachedTranspilerCodeBlockNode;
import com.mojo.algorithms.transpiler.IfTranspilerNode;
import com.mojo.algorithms.transpiler.TranspilerCodeBlockNode;
import com.mojo.algorithms.transpiler.TranspilerNode;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.ConditionalStatementFlowNode;
import org.smojol.toolkit.ast.IfFlowNode;
import org.smojol.toolkit.intermediate.SectionParagraphMap;

public class IfTranspilerNodeBuilder {
    public static TranspilerNode build(IfFlowNode n, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        CobolExpression condition = n.getConditionExpression();
        TranspilerExpressionBuilder nodeBuilder = new TranspilerExpressionBuilder(dataStructures);
        TranspilerNode transpilerCondition = nodeBuilder.build(condition);
        TranspilerCodeBlockNode ifThenBlock = new DetachedTranspilerCodeBlockNode(n.getIfThenBlock().astChildren().stream().map(stmt -> TranspilerTreeBuilder.flowToTranspiler(stmt, dataStructures, sectionParagraphMap)).toList());
        TranspilerCodeBlockNode ifElseBlock = n.getIfElseBlock() != null ? new DetachedTranspilerCodeBlockNode(n.getIfElseBlock().astChildren().stream().filter(c -> c instanceof ConditionalStatementFlowNode).map(stmt -> TranspilerTreeBuilder.flowToTranspiler(stmt, dataStructures, sectionParagraphMap)).toList()) : new TranspilerCodeBlockNode();
        return new IfTranspilerNode(transpilerCondition, ifThenBlock, ifElseBlock);
    }
}
