package org.smojol.common.vm.structure;

public class CurriedIndexedAccess implements AccessLink {
    @Override
    public CobolDataStructure run(CobolDataStructure structure, IndexProvider indexProvider) {
        return structure.cobolIndex(indexProvider.next());
    }
}
