package org.smojol.common.ast;

import lombok.Getter;

@Getter
public class SerialisableCFGFlowNode {
    private final String id;
    private final String label;
    private final String name;
    private final String originalText;
    private final FlowNodeType type;
    private final String nodeType = "CODE_VERTEX";

    protected SerialisableCFGFlowNode(String id, String label, String name, String originalText, FlowNodeType type) {
        this.id = id;
        this.label = label;
        this.name = name;
        this.originalText = originalText;
        this.type = type;
    }

    public SerialisableCFGFlowNode(FlowNode current) {
        this(current.id(), current.label(), current.name(), current.originalText(), current.type());
    }
}
