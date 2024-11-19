package org.smojol.toolkit.transpiler;

import com.google.common.collect.ImmutableMap;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.ast.NullFlowNode;
import org.smojol.common.transpiler.LabelledTranspilerCodeBlockNode;
import org.smojol.common.transpiler.NullTranspilerNode;
import org.smojol.common.transpiler.RetCallTranspilerNode;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.task.transpiler.SectionParagraphMap;
import org.smojol.toolkit.ast.ParagraphFlowNode;
import org.smojol.toolkit.ast.ProcedureDivisionBodyFlowNode;
import org.smojol.toolkit.ast.SectionFlowNode;

import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

public class LabelledTranspilerCodeBlockNodeBuilder {
    public static TranspilerNode build(SectionFlowNode n, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        FlowNode next = sectionParagraphMap.nextSection(n).get();
        return labelledBlock(n, dataStructures, ImmutableMap.of("type", FlowNodeType.SECTION), sectionParagraphMap, next);
    }

    private static LabelledTranspilerCodeBlockNode labelledBlock(FlowNode n, CobolDataStructure dataStructures, Map<String, Object> properties, SectionParagraphMap sectionParagraphMap, FlowNode next) {
        Stream<TranspilerNode> childTranspilerNodes = n.astChildren().stream().map(child -> TranspilerTreeBuilder.flowToTranspiler(child, dataStructures, sectionParagraphMap));
        List<TranspilerNode> childNodesWithRetCall = Stream.concat(childTranspilerNodes, next instanceof NullTranspilerNode
                ? Stream.of()
                : Stream.of(new RetCallTranspilerNode(next.label()))).toList();
        return new LabelledTranspilerCodeBlockNode(n.label(), childNodesWithRetCall, properties);
    }

    public static TranspilerNode build(ParagraphFlowNode n, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        ParagraphFlowNode next = sectionParagraphMap.nextParagraph(n).get();
        return labelledBlock(n, dataStructures, ImmutableMap.of("type", FlowNodeType.PARAGRAPH), sectionParagraphMap, next);
    }

    public static TranspilerNode build(ProcedureDivisionBodyFlowNode n, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        return labelledBlock(n, dataStructures, ImmutableMap.of("type", FlowNodeType.PROCEDURE_DIVISION_BODY), sectionParagraphMap, new NullFlowNode());
    }

//    public static TranspilerNode build(ParagraphsFlowNode n, CobolDataStructure dataStructures) {
//        List<TranspilerNode> childTranspilerNodes = n.astChildren().stream().map(child -> TranspilerTreeBuilder.flowToTranspiler(child, dataStructures, sectionParagraphMap)).toList();
//        return new TranspilerCodeBlockNode(childTranspilerNodes);
//    }
}
