package org.smojol.toolkit.transpiler;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.CodeSentinelType;
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
    public static TranspilerNode build(SectionFlowNode n, CobolDataStructure dataStructures, CodeSentinelType codeSentinelType) {
        return labelledBlock(n, dataStructures, codeSentinelType);
    }

    private static LabelledTranspilerCodeBlockNode labelledBlock(FlowNode n, CobolDataStructure dataStructures, CodeSentinelType codeSentinelType) {
        List<TranspilerNode> childTranspilerNodes = n.astChildren().stream().map(child -> TranspilerTreeBuilder.flowToTranspiler(child, dataStructures, codeSentinelType)).toList();
        return new LabelledTranspilerCodeBlockNode(n.name(), childTranspilerNodes, codeSentinelType);
    }

    public static TranspilerNode build(ParagraphFlowNode n, CobolDataStructure dataStructures, CodeSentinelType codeSentinelType) {
        return labelledBlock(n, dataStructures, codeSentinelType);
    }

    public static TranspilerNode build(ProcedureDivisionBodyFlowNode n, CobolDataStructure dataStructures) {
        return labelledBlock(n, dataStructures, CodeSentinelType.BODY);
    }

    public static TranspilerNode build(ParagraphsFlowNode n, CobolDataStructure dataStructures) {
        List<TranspilerNode> childTranspilerNodes = n.astChildren().stream().map(child -> TranspilerTreeBuilder.flowToTranspiler(child, dataStructures, CodeSentinelType.BODY)).toList();
        return new TranspilerCodeBlock(childTranspilerNodes);
    }
}
