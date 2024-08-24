package org.smojol.common.ast;


public class SerialisableFlowNodeASTVisitor extends FlowNodeASTVisitor<SerialisableASTFlowNode> {
    public SerialisableFlowNodeASTVisitor(SerialisableASTFlowNode ancestorNode) {
        super(ancestorNode);
    }

    public SerialisableFlowNodeASTVisitor() {
        this(null);
    }

    @Override
    public FlowNodeASTVisitor<SerialisableASTFlowNode> visit(FlowNode node) {
        SerialisableASTFlowNode child = new SerialisableASTFlowNode(node);
        if (ancestor == null) return new SerialisableFlowNodeASTVisitor(child);
        ancestor.addChild(child);
        return new SerialisableFlowNodeASTVisitor(child);
    }
}
