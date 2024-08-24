package org.smojol.common.ast;


public class SerialisableFlowNodeASTVisitor extends FlowNodeASTVisitor<SerialisableASTFlowNode> {
    public SerialisableFlowNodeASTVisitor(SerialisableASTFlowNode ancestorNode) {
        super(ancestorNode);
    }

    public SerialisableFlowNodeASTVisitor() {
        this(null);
    }

    @Override
    public SerialisableASTFlowNode visit(FlowNode node) {
        SerialisableASTFlowNode child = new SerialisableASTFlowNode(node);
        if (ancestor == null) return child;
        ancestor.addChild(child);
        return child;
    }

    @Override
    public FlowNodeASTVisitor<SerialisableASTFlowNode> scope(FlowNode n, SerialisableASTFlowNode visitResult) {
        return new SerialisableFlowNodeASTVisitor(visitResult);
    }
}
