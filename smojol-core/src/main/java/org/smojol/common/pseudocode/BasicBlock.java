package org.smojol.common.pseudocode;

import java.util.ArrayList;
import java.util.List;

public class BasicBlock {
    private final List<PseudocodeInstruction> instructions = new ArrayList<>();
    private final String id;

    public BasicBlock(String id) {
        this.id = id;
    }

    public void add(PseudocodeInstruction instruction) {
        instructions.add(instruction);
    }

    public boolean isEmpty() {
        return instructions.isEmpty();
    }
}
