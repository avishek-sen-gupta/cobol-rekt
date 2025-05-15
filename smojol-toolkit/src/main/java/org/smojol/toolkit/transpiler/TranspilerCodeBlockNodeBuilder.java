package org.smojol.toolkit.transpiler;

import com.google.common.collect.ImmutableMap;
import com.mojo.algorithms.domain.FlowNodeType;
import com.mojo.algorithms.transpiler.TranspilerCodeBlockNode;
import com.mojo.algorithms.domain.TranspilerNode;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.ParagraphsFlowNode;
import org.smojol.toolkit.ast.SectionFlowNode;
import org.smojol.toolkit.ast.SentenceFlowNode;
import org.smojol.toolkit.intermediate.SectionParagraphMap;

import java.util.List;
import java.util.Map;

public class TranspilerCodeBlockNodeBuilder {
    public static TranspilerNode build(SectionFlowNode n, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        return block(n, dataStructures, sectionParagraphMap);
    }

    private static TranspilerCodeBlockNode block(FlowNode n, CobolDataStructure dataStructures, Map<String, Object> additionalAttributes, SectionParagraphMap sectionParagraphMap) {
        List<TranspilerNode> childTranspilerNodes = n.astChildren().stream().map(child -> TranspilerTreeBuilder.flowToTranspiler(child, dataStructures, sectionParagraphMap)).toList();
        return new TranspilerCodeBlockNode(childTranspilerNodes, additionalAttributes);
    }

    private static TranspilerCodeBlockNode block(FlowNode n, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        return block(n, dataStructures, ImmutableMap.of(), sectionParagraphMap);
    }

    public static TranspilerNode build(ParagraphsFlowNode n, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        return block(n, dataStructures, sectionParagraphMap);
    }

    public static TranspilerNode build(SentenceFlowNode n, CobolDataStructure dataStructures, SectionParagraphMap sectionParagraphMap) {
        return block(n, dataStructures, ImmutableMap.of("type", FlowNodeType.SENTENCE), sectionParagraphMap);
    }
}
