package org.smojol.toolkit.transpiler;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.transpiler.TranspilerCodeBlock;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.*;

import java.util.List;

public class TranspilerCodeBlockNodeBuilder {
    public static TranspilerNode build(SectionFlowNode n, CobolDataStructure dataStructures, CodeSentinelType codeSentinelType) {
        return block(n, dataStructures, codeSentinelType);
    }

    private static TranspilerCodeBlock block(FlowNode n, CobolDataStructure dataStructures, CodeSentinelType codeSentinelType) {
        List<TranspilerNode> childTranspilerNodes = n.astChildren().stream().map(child -> TranspilerTreeBuilder.flowToTranspiler(child, dataStructures, codeSentinelType)).toList();
        return new TranspilerCodeBlock(childTranspilerNodes);
    }

    public static TranspilerNode build(ParagraphsFlowNode n, CobolDataStructure dataStructures) {
        return block(n, dataStructures, CodeSentinelType.BODY).unwrap();
    }

    public static TranspilerNode build(SentenceFlowNode n, CobolDataStructure dataStructures) {
        return block(n, dataStructures, CodeSentinelType.BODY).unwrap();
    }
}
