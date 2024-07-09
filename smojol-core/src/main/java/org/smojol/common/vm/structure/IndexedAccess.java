package org.smojol.common.vm.structure;

public class IndexedAccess implements AccessLink {
    private final int index;

    public IndexedAccess(int index) {
        this.index = index;
    }

    @Override
    public CobolDataStructure run(CobolDataStructure structure, IndexProvider indexProvider) {
        return structure.index(index);
    }
}
