package org.smojol.common.pseudocode;

import lombok.Getter;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.ast.FlowNode;

public class PseudocodeInstruction {
    @Getter private final FlowNode node;
    private final PseudocodeMetatype metatype;
    private Pair<Integer, Integer> range;

    public PseudocodeInstruction(FlowNode node, PseudocodeMetatype metatype) {
        this.metatype = metatype;
        this.node = node;
    }

    @Override
    public String toString() {
        return metatype + " / " + node.label();
    }

    public boolean isBody() {
        return metatype == PseudocodeMetatype.BODY;
    }
}
