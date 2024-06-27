package org.smojol.common.vm.strategy;

import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

public interface UnresolvedReferenceStrategy {
    void runIfResolved(List<? extends CobolDataStructure> path, String recordID, Runnable r);
}
