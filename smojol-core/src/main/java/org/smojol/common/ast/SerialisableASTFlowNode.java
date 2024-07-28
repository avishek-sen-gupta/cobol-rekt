package org.smojol.common.ast;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public class SerialisableASTFlowNode {
    List<SerialisableASTFlowNode> children = new ArrayList<>();

    String id;
    String label;
    String name;
    String originalText;
    FlowNodeType type;

    public SerialisableASTFlowNode() {
        this.id = "ROOT";
        this.label = "ROOT";
        this.name = "ROOT";
        this.originalText = "[ROOT]";
        this.type = FlowNodeType.DUMMY;
    }

    public SerialisableASTFlowNode(FlowNode current) {
        id = current.id();
        label = current.label();
        name = current.name();
        originalText = current.originalText();
        type = current.type();
    }

    public void addChild(SerialisableASTFlowNode child) {
        children.add(child);
    }
}
