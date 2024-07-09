package org.smojol.common.vm.structure;

public interface AccessLink {
    CobolDataStructure run(CobolDataStructure structure, IndexProvider indexProvider);
}
