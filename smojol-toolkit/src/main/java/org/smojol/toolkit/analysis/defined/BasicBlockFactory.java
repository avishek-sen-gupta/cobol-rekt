package org.smojol.toolkit.analysis.defined;

import org.smojol.common.id.IdProvider;
import org.smojol.common.pseudocode.PseudocodeInstruction;

import java.util.ArrayList;
import java.util.List;

public class BasicBlockFactory {
    private final IdProvider idProvider;

    public BasicBlockFactory(IdProvider idProvider) {
        this.idProvider = idProvider;
    }


    public BasicBlock block() {
        return new BasicBlock(idProvider.next());
    }
}
