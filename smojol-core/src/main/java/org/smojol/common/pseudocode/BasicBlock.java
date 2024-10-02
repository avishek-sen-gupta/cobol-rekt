package org.smojol.common.pseudocode;

import org.smojol.common.id.Identifiable;

import java.util.ArrayList;
import java.util.List;

public class BasicBlock<T> implements Identifiable {
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

    public int size() {
        return instructions.size();
    }

    public T lastInstruction() {
        return instructions.getLast();
    }

    @Override
    public String id() {
        return id;
    }

    @Override
    public String label() {
        return id;
//        return String.join("\n", instructions.stream().map(Object::toString).toList());
    }

    public Boolean contains(T instruction) {
        return instructions.contains(instruction);
    }
}
