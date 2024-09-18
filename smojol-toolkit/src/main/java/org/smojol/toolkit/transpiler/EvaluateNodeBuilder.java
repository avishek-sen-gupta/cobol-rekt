package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.*;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.EvaluateFlowNode;

public class EvaluateNodeBuilder {
    public static TranspilerNode build(EvaluateFlowNode n, CobolDataStructure dataStructures) {
        return new PlaceholderTranspilerNode(n.originalText());
    }
}
