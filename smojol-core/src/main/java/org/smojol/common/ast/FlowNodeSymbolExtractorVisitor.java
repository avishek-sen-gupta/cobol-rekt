package org.smojol.common.ast;

import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.structure.CobolDataStructure;

@Deprecated
public class FlowNodeSymbolExtractorVisitor extends FlowNodeASTVisitor<FlowNode> {
    private final SmojolSymbolTable symbolTable;
    private final CobolDataStructure dataStructures;

    public FlowNodeSymbolExtractorVisitor(FlowNode ancestor, CobolDataStructure dataStructures, SmojolSymbolTable symbolTable) {
        super(ancestor);
        this.symbolTable = symbolTable;
        this.dataStructures = dataStructures;
    }

    @Override
    public FlowNode visit(FlowNode node) {
        node.resolve(symbolTable, dataStructures);
        return null;
    }

    @Override
    public FlowNodeASTVisitor<FlowNode> scope(FlowNode n, FlowNode visitResult) {
        return this;
    }
}
