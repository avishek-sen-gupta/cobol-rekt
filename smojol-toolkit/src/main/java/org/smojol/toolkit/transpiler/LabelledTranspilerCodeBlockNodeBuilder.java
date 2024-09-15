package org.smojol.toolkit.transpiler;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.transpiler.LabelledTranspilerCodeBlockNode;
import org.smojol.common.transpiler.TranspilerCodeBlock;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.ParagraphFlowNode;
import org.smojol.toolkit.ast.ParagraphsFlowNode;
import org.smojol.toolkit.ast.ProcedureDivisionBodyFlowNode;
import org.smojol.toolkit.ast.SectionFlowNode;

import java.util.List;

public class LabelledTranspilerCodeBlockNodeBuilder {
    public static TranspilerNode build(SectionFlowNode n, CobolDataStructure dataStructures) {
        return labelledBlock(n, dataStructures);
    }

    private static LabelledTranspilerCodeBlockNode labelledBlock(FlowNode n, CobolDataStructure dataStructures) {
        List<TranspilerNode> childTranspilerNodes = n.astChildren().stream().map(child -> TranspilerTreeBuilder.flowToTranspiler(child, dataStructures)).toList();
        return new LabelledTranspilerCodeBlockNode(n.name(), childTranspilerNodes);
    }

    public static TranspilerNode build(ParagraphFlowNode n, CobolDataStructure dataStructures) {
        return labelledBlock(n, dataStructures);
    }

    public static TranspilerNode build(ProcedureDivisionBodyFlowNode n, CobolDataStructure dataStructures) {
        return labelledBlock(n, dataStructures);
    }

    public static TranspilerNode build(ParagraphsFlowNode n, CobolDataStructure dataStructures) {
        List<TranspilerNode> childTranspilerNodes = n.astChildren().stream().map(child -> TranspilerTreeBuilder.flowToTranspiler(child, dataStructures)).toList();
        return new TranspilerCodeBlock(childTranspilerNodes);
    }
}
