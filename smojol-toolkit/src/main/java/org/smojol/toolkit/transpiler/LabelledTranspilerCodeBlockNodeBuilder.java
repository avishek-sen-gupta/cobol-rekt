package org.smojol.toolkit.transpiler;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.ParagraphFlowNode;
import org.smojol.toolkit.ast.SectionFlowNode;

public class LabelledTranspilerCodeBlockNodeBuilder {
    public static TranspilerNode build(SectionFlowNode n, CobolDataStructure dataStructures, CodeSentinelType codeSentinelType) {
        return labelledBlock(n, dataStructures, codeSentinelType);
    }

    private static LabelledTranspilerCodeBlockNode labelledBlock(FlowNode n, CobolDataStructure dataStructures, CodeSentinelType codeSentinelType) {
        return new LabelledTranspilerCodeBlockNode(n.name(), codeSentinelType);
    }

    public static TranspilerNode build(ParagraphFlowNode n, CobolDataStructure dataStructures, CodeSentinelType codeSentinelType) {
        return labelledBlock(n, dataStructures, codeSentinelType);
    }
}
