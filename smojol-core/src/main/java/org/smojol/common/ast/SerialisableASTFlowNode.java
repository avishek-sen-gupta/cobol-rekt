package org.smojol.common.ast;

import com.google.common.collect.ImmutableList;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

@Getter
public class SerialisableASTFlowNode extends SerialisableCFGFlowNode {
    List<SerialisableASTFlowNode> children = new ArrayList<>();

    public SerialisableASTFlowNode() {
        super("ROOT", "ROOT", "ROOT", "ROOT", FlowNodeType.DUMMY, ImmutableList.of(SemanticCategory.CODE_ROOT));
    }

    public SerialisableASTFlowNode(FlowNode current) {
        super(current);
    }

    public void addChild(SerialisableASTFlowNode child) {
        children.add(child);
    }
}
