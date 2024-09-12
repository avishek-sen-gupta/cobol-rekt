package org.smojol.toolkit.transpiler;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.*;

public class TranspilerTreeBuilder {
    public static TranspilerNode flowToTranspiler(FlowNode node, CobolDataStructure dataStructures) {
        return switch (node) {
            case MoveFlowNode n -> SetTranspilerNodeBuilder.build(n, dataStructures);
            case IfFlowNode n -> IfTranspilerNodeBuilder.build(n, dataStructures);
            case AddFlowNode n -> AddTranspilerNodeBuilder.build(n, dataStructures);
            case SubtractFlowNode n -> SubtractTranspilerNodeBuilder.build(n, dataStructures);
            case MultiplyFlowNode n -> MultiplyTranspilerNodeBuilder.build(n, dataStructures);
            case DivideFlowNode n -> DivideTranspilerNodeBuilder.build(n, dataStructures);
            case ComputeFlowNode n -> ComputeTranspilerNodeBuilder.build(n, dataStructures);
            case DisplayFlowNode n -> DisplayTranspilerNodeBuilder.build(n, dataStructures);
            case ConditionalStatementFlowNode n -> flowToTranspiler(n.astChildren().getFirst(), dataStructures);
            default -> new TranspilerCodeBlock();
        };
    }
}
