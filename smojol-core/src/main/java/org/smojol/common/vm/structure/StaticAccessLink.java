package org.smojol.common.vm.structure;

public class StaticAccessLink implements AccessLink {
    private final CobolDataStructure structure;

    public StaticAccessLink(CobolDataStructure structure) {
        this.structure = structure;
    }

    @Override
    public CobolDataStructure run(CobolDataStructure structure, IndexProvider indexProvider) {
        return this.structure;
    }
}
