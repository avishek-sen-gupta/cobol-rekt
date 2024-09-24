package org.smojol.common.pseudocode;

import org.smojol.common.id.IdProvider;
import org.smojol.common.vm.structure.CobolDataStructure;

public class SymbolReferenceBuilder {
    private final IdProvider idProvider;

    public SymbolReferenceBuilder(IdProvider idProvider) {
        this.idProvider = idProvider;
    }

    public SymbolReference recordReference(CobolDataStructure record) {
        return new RecordSymbolReference(record, record.name());
    }
}
