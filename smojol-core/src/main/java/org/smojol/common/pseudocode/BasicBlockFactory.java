package org.smojol.common.pseudocode;

import org.smojol.common.id.IdProvider;

public class BasicBlockFactory {
    private final IdProvider idProvider;

    public BasicBlockFactory(IdProvider idProvider) {
        this.idProvider = idProvider;
    }


    public BasicBlock block() {
        return new BasicBlock(idProvider.next());
    }
}
