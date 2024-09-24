package org.smojol.common.pseudocode;

import java.util.ArrayList;
import java.util.List;

public class BasicBlock<T> {
    private final List<T> instructions = new ArrayList<>();
    private final String id;

    public BasicBlock(String id) {
        this.id = id;
    }

    public void add(T instruction) {
        instructions.add(instruction);
    }

    public boolean isEmpty() {
        return instructions.isEmpty();
    }
}
