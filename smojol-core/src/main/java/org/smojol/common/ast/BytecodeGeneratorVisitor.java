package org.smojol.common.ast;

public class BytecodeGeneratorVisitor extends FlowNodeASTVisitor<FlowNode> {
    public BytecodeGeneratorVisitor(FlowNode ancestor) {
        super(ancestor);
    }

    @Override
    public FlowNodeASTVisitor<FlowNode> visit(FlowNode node) {
        return this;
    }
}
