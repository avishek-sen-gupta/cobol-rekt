package org.smojol.common.transpiler;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeASTTraversal;
import org.smojol.common.ast.FlowNodeSymbolExtractorVisitor;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.structure.CobolDataStructure;

public class TranspilerSetup {
    public static void buildSymbolTable(FlowNode astRoot, CobolDataStructure dataStructRoot, SmojolSymbolTable symbolTable) {
        new FlowNodeASTTraversal<FlowNode>().accept(astRoot, new FlowNodeSymbolExtractorVisitor(astRoot, dataStructRoot, symbolTable));
    }
}
