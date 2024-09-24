package org.smojol.common.pseudocode;

import org.smojol.common.id.IdProvider;

public class BasicBlockFactory<T> {
    private final IdProvider idProvider;

    public BasicBlockFactory(IdProvider idProvider) {
        this.idProvider = idProvider;
    }


    public BasicBlock<T> block() {
        return new BasicBlock<T>(idProvider.next());
    }
}
