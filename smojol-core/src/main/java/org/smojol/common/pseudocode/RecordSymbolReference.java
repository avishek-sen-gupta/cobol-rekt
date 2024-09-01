package org.smojol.common.pseudocode;

import org.smojol.common.vm.structure.CobolDataStructure;

public class RecordSymbolReference extends SymbolReference {
    private final CobolDataStructure record;

    public RecordSymbolReference(CobolDataStructure record, String id) {
        super(id);
        this.record = record;
    }
}
