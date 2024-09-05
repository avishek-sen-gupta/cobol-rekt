package org.smojol.toolkit.analysis.defined;

import org.smojol.common.pseudocode.PseudocodeInstruction;

import java.util.ArrayList;
import java.util.List;

public class BasicBlock {
    private final List<PseudocodeInstruction> instructions = new ArrayList<>();

    public void add(PseudocodeInstruction instruction) {
        instructions.add(instruction);
    }

    public boolean isEmpty() {
        return instructions.isEmpty();
    }
}
