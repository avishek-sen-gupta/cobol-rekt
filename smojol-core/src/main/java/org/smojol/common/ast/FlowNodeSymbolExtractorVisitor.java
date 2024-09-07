package org.smojol.common.ast;

import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.expression.CobolExpressionBuilder;
import org.smojol.common.vm.structure.CobolDataStructure;

public class FlowNodeSymbolExtractorVisitor extends FlowNodeASTVisitor<FlowNode> {
    private final SmojolSymbolTable symbolTable;
    private final CobolDataStructure dataStructures;

    public FlowNodeSymbolExtractorVisitor(FlowNode ancestor, SmojolSymbolTable symbolTable, CobolDataStructure dataStructures) {
        super(ancestor);
        this.symbolTable = symbolTable;
        this.dataStructures = dataStructures;
        new CobolExpressionBuilder();
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