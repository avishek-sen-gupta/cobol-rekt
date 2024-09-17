package org.smojol.common.pseudocode;

import lombok.Getter;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.ast.*;

import java.util.List;

public class PseudocodeInstruction implements FlowNodeLike {
    public static final PseudocodeInstruction NULL = new PseudocodeInstruction(new NullFlowNode(), CodeSentinelType.BODY, "42");
    @Getter
    private final FlowNode node;
    private final String id;
    @Getter
    private final CodeSentinelType sentinelType;
    private Pair<Integer, Integer> range;

    public PseudocodeInstruction(FlowNode node, CodeSentinelType sentinelType, String uuid) {
        this.sentinelType = sentinelType;
        this.node = node;
        this.id = uuid;
    }

    public <T> T typedNode(Class<T> nodeType) {
        return nodeType.cast(node);
    }

    @Override
    public String toString() {
        return label();
    }

    public boolean isBody() {
        return sentinelType == CodeSentinelType.BODY;
    }

    public boolean isJump() {
        return node.categories().contains(SemanticCategory.CONTROL_FLOW);
    }

    public boolean isCondition() {
        return node.categories().contains(SemanticCategory.DECISION);
    }

    @Override
    public String id() {
        return id;
    }

    @Override
    public String label() {
        return sentinelType + " / " + node.label();
    }

    @Override
    public String name() {
        return node.name();
    }

    @Override
    public String originalText() {
        return node.originalText();
    }

    @Override
    public FlowNodeType type() {
        return node.type();
    }

    @Override
    public List<SemanticCategory> categories() {
        return node.categories();
    }

    @Override
    public CodeSentinelType codeSentinelType() {
        return sentinelType;
    }
}
