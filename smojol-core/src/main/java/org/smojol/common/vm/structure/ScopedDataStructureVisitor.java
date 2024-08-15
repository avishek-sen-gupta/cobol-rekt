package org.smojol.common.vm.structure;

public interface ScopedDataStructureVisitor {
    ScopedDataStructureVisitor visit(CobolDataStructure data);
}
