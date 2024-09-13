package org.smojol.toolkit.transpiler;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.transpiler.TranspilerCodeBlock;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.*;

public class TranspilerTreeBuilder {
    public static TranspilerNode flowToTranspiler(FlowNode node, CobolDataStructure dataStructures, CodeSentinelType codeSentinelType) {
        return switch (node) {
            case MoveFlowNode n -> SetTranspilerNodeBuilder.build(n, dataStructures);
            case IfFlowNode n -> IfTranspilerNodeBuilder.build(n, dataStructures, codeSentinelType);
            case AddFlowNode n -> AddTranspilerNodeBuilder.build(n, dataStructures);
            case SubtractFlowNode n -> SubtractTranspilerNodeBuilder.build(n, dataStructures);
            case MultiplyFlowNode n -> MultiplyTranspilerNodeBuilder.build(n, dataStructures);
            case DivideFlowNode n -> DivideTranspilerNodeBuilder.build(n, dataStructures);
            case ComputeFlowNode n -> ComputeTranspilerNodeBuilder.build(n, dataStructures);
            case DisplayFlowNode n -> DisplayTranspilerNodeBuilder.build(n, dataStructures);
            case ConditionalStatementFlowNode n -> flowToTranspiler(n.astChildren().getFirst(), dataStructures, codeSentinelType);
            case SectionFlowNode n -> LabelledTranspilerCodeBlockNodeBuilder.build(n, dataStructures, codeSentinelType);
            case ParagraphFlowNode n -> LabelledTranspilerCodeBlockNodeBuilder.build(n, dataStructures, codeSentinelType);
            case GoToFlowNode n -> JumpNodeBuilder.build(n, dataStructures);
            case PerformProcedureFlowNode n -> PerformProcedureNodeBuilder.build(n, dataStructures);
            case PerformInlineFlowNode n -> PerformProcedureNodeBuilder.build(n, dataStructures);
            default -> new TranspilerCodeBlock();
        };
    }
}
