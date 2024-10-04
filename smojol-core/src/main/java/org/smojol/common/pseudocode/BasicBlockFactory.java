package org.smojol.common.pseudocode;

import org.smojol.common.id.IdProvider;
import org.smojol.common.id.Identifiable;

public class BasicBlockFactory<T extends Identifiable> {
    private final IdProvider idProvider;

    public BasicBlockFactory(IdProvider idProvider) {
        this.idProvider = idProvider;
    }

    public BasicBlock<T> block() {
        return new BasicBlock<T>(idProvider.next());
    }
}
