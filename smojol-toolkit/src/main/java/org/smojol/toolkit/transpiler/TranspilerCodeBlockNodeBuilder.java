package org.smojol.toolkit.transpiler;

import com.google.common.collect.ImmutableMap;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.transpiler.TranspilerCodeBlock;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.*;

import java.util.List;
import java.util.Map;

public class TranspilerCodeBlockNodeBuilder {
    public static TranspilerNode build(SectionFlowNode n, CobolDataStructure dataStructures) {
        return block(n, dataStructures);
    }

    private static TranspilerCodeBlock block(FlowNode n, CobolDataStructure dataStructures, Map<String, Object> additionalAttributes) {
        List<TranspilerNode> childTranspilerNodes = n.astChildren().stream().map(child -> TranspilerTreeBuilder.flowToTranspiler(child, dataStructures)).toList();
        return new TranspilerCodeBlock(childTranspilerNodes, additionalAttributes);
    }

    private static TranspilerCodeBlock block(FlowNode n, CobolDataStructure dataStructures) {
        return block(n, dataStructures, ImmutableMap.of());
    }

    public static TranspilerNode build(ParagraphsFlowNode n, CobolDataStructure dataStructures) {
        return block(n, dataStructures);
    }

    public static TranspilerNode build(SentenceFlowNode n, CobolDataStructure dataStructures) {
        return block(n, dataStructures, ImmutableMap.of("type", FlowNodeType.SENTENCE));
    }
}
