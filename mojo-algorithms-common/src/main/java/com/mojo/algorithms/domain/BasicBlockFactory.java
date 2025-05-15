package com.mojo.algorithms.domain;

import com.mojo.algorithms.id.IdProvider;
import com.mojo.algorithms.id.InstructionLike;

public class BasicBlockFactory<T extends InstructionLike> {
    private final IdProvider idProvider;

    public BasicBlockFactory(IdProvider idProvider) {
        this.idProvider = idProvider;
    }

    public BasicBlock<T> block() {
        return new BasicBlock<T>(idProvider.next());
    }
}
