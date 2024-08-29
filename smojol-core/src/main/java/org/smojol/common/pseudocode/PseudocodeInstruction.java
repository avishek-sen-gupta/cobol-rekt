package org.smojol.common.pseudocode;

import lombok.Getter;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeCategory;

public class PseudocodeInstruction {
    public static final PseudocodeInstruction NULL = new PseudocodeInstruction(null, PseudocodeMetatype.NO_OP, "42");
    @Getter private final FlowNode node;
    private final String id;
    @Getter private final PseudocodeMetatype metatype;
    private Pair<Integer, Integer> range;

    public PseudocodeInstruction(FlowNode node, PseudocodeMetatype metatype, String uuid) {
        this.metatype = metatype;
        this.node = node;
        this.id = uuid;
    }

    @Override
    public String toString() {
        return metatype + " / " + node.label();
    }

    public boolean isBody() {
        return metatype == PseudocodeMetatype.BODY;
    }

    public boolean isJump() {
        return node.categories().contains(FlowNodeCategory.CONTROL_FLOW);
    }

    public boolean isCondition() {
        return node.categories().contains(FlowNodeCategory.DECISION);
    }
}
