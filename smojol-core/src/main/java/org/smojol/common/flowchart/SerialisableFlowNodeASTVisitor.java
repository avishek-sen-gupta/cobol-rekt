package org.smojol.common.flowchart;


public class SerialisableFlowNodeASTVisitor implements FlowNodeASTVisitor<SerialisableASTFlowNode> {
    SerialisableASTFlowNode root = new SerialisableASTFlowNode();
    @Override
    public SerialisableASTFlowNode visit(FlowNode node, SerialisableASTFlowNode parent) {
        SerialisableASTFlowNode child = new SerialisableASTFlowNode(node);
        parent.addChild(child);
        return child;
    }

    @Override
    public SerialisableASTFlowNode root() {
        return root;
    }
}
