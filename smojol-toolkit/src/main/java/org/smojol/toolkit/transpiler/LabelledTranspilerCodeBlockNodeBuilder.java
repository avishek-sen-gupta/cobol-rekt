package org.smojol.toolkit.transpiler;

import com.google.common.collect.ImmutableMap;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.transpiler.LabelledTranspilerCodeBlockNode;
import org.smojol.common.transpiler.TranspilerCodeBlock;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.ParagraphFlowNode;
import org.smojol.toolkit.ast.ParagraphsFlowNode;
import org.smojol.toolkit.ast.ProcedureDivisionBodyFlowNode;
import org.smojol.toolkit.ast.SectionFlowNode;

import java.util.List;
import java.util.Map;

public class LabelledTranspilerCodeBlockNodeBuilder {
    public static TranspilerNode build(SectionFlowNode n, CobolDataStructure dataStructures) {
        return labelledBlock(n, dataStructures);
    }

    private static LabelledTranspilerCodeBlockNode labelledBlock(FlowNode n, CobolDataStructure dataStructures) {
        return labelledBlock(n, dataStructures, ImmutableMap.of());
    }

    private static LabelledTranspilerCodeBlockNode labelledBlock(FlowNode n, CobolDataStructure dataStructures, Map<String, Object> properties) {
        List<TranspilerNode> childTranspilerNodes = n.astChildren().stream().map(child -> TranspilerTreeBuilder.flowToTranspiler(child, dataStructures)).toList();
        return new LabelledTranspilerCodeBlockNode(n.name(), childTranspilerNodes, properties);
    }

    public static TranspilerNode build(ParagraphFlowNode n, CobolDataStructure dataStructures) {
        return labelledBlock(n, dataStructures);
    }

    public static TranspilerNode build(ProcedureDivisionBodyFlowNode n, CobolDataStructure dataStructures) {
        return labelledBlock(n, dataStructures, ImmutableMap.of("type", FlowNodeType.PROCEDURE_DIVISION_BODY));
    }

    public static TranspilerNode build(ParagraphsFlowNode n, CobolDataStructure dataStructures) {
        List<TranspilerNode> childTranspilerNodes = n.astChildren().stream().map(child -> TranspilerTreeBuilder.flowToTranspiler(child, dataStructures)).toList();
        return new TranspilerCodeBlock(childTranspilerNodes);
    }
}
