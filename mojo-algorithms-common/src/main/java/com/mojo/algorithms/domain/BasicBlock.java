package com.mojo.algorithms.domain;

import com.mojo.algorithms.id.InstructionLike;
import lombok.Getter;
import com.mojo.algorithms.id.Identifiable;

import java.util.ArrayList;
import java.util.List;

public class BasicBlock<T extends InstructionLike> implements Identifiable {
    @Getter private final List<T> instructions = new ArrayList<>();
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

    public boolean contains(T instruction, CodeSentinelType codeSentinelType) {
        return instructions.contains(instruction) && instruction.sentinel() == codeSentinelType;
    }
}
