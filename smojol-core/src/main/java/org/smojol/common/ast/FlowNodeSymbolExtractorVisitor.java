package org.smojol.common.ast;

import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.vm.expression.CobolExpressionBuilder;

public class FlowNodeSymbolExtractorVisitor extends FlowNodeASTVisitor<FlowNode> {
    private final SmojolSymbolTable symbolTable;

    public FlowNodeSymbolExtractorVisitor(FlowNode ancestor, SmojolSymbolTable symbolTable) {
        super(ancestor);
        this.symbolTable = symbolTable;
        new CobolExpressionBuilder();
    }

    @Override
    public FlowNode visit(FlowNode node) {
        node.resolve(symbolTable);
        return null;
    }

    @Override
    public FlowNodeASTVisitor<FlowNode> scope(FlowNode n, FlowNode visitResult) {
        return this;
    }
}
