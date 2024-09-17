package org.smojol.common.ast;

import lombok.Getter;

import java.util.List;

@Getter
public class SerialisableCFGFlowNode {
    private final String id;
    private final String label;
    private final String name;
    private final String originalText;
    private final FlowNodeType type;
    private final List<SemanticCategory> categories;
    private final String nodeType = "CODE_VERTEX";

    protected SerialisableCFGFlowNode(String id, String label, String name, String originalText, FlowNodeType type, List<SemanticCategory> categories) {
        this.id = id;
        this.label = label;
        this.name = name;
        this.originalText = originalText;
        this.type = type;
        this.categories = categories;
    }

    public SerialisableCFGFlowNode(FlowNodeLike current) {
        this(current.id(), current.label(), current.name(), current.originalText(), current.type(), current.categories());
    }
}
