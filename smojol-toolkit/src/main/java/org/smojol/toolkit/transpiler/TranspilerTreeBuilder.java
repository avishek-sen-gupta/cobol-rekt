package org.smojol.toolkit.transpiler;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.AddFlowNode;
import org.smojol.toolkit.ast.IfFlowNode;
import org.smojol.toolkit.ast.MoveFlowNode;
import org.smojol.toolkit.ast.SubtractFlowNode;

public class TranspilerTreeBuilder {
    public static TranspilerNode flowToTranspiler(FlowNode node, CobolDataStructure dataStructures) {
        return switch (node) {
            case MoveFlowNode n -> SetTranspilerNodeBuilder.build(n, dataStructures);
            case IfFlowNode n -> IfTranspilerNodeBuilder.build(n, dataStructures);
            case AddFlowNode n -> AddTranspilerNodeBuilder.build(n, dataStructures);
            case SubtractFlowNode n -> SubtractTranspilerNodeBuilder.build(n, dataStructures);
            default -> new TranspilerCodeBlock();
        };
    }
}
