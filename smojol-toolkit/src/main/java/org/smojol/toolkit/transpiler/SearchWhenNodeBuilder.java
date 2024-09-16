package org.smojol.toolkit.transpiler;

import org.smojol.common.transpiler.PlaceholderTranspilerNode;
import org.smojol.common.transpiler.TranspilerNode;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.ast.SearchFlowNode;

public class SearchWhenNodeBuilder {
    public static TranspilerNode build(SearchFlowNode n, CobolDataStructure dataStructures) {
        return new PlaceholderTranspilerNode(n.originalText());
    }
}
