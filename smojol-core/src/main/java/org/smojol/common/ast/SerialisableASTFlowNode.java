package org.smojol.common.ast;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public class SerialisableASTFlowNode extends SerialisableCFGFlowNode {
    List<SerialisableASTFlowNode> children = new ArrayList<>();

    public SerialisableASTFlowNode() {
        super("ROOT", "ROOT", "ROOT", "ROOT", FlowNodeType.DUMMY);
    }

    public SerialisableASTFlowNode(FlowNode current) {
        super(current);
    }

    public void addChild(SerialisableASTFlowNode child) {
        children.add(child);
    }
}
