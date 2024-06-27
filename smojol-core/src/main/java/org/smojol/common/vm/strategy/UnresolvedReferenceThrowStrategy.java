package org.smojol.common.vm.strategy;

import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;
import java.util.NoSuchElementException;

public class UnresolvedReferenceThrowStrategy implements UnresolvedReferenceStrategy {
    @Override
    public void runIfResolved(List<? extends CobolDataStructure> path, String recordID, Runnable r) {
        if (path.isEmpty()) throw new NoSuchElementException(String.format("%s is not a valid variable", recordID));
        r.run();
    }

}
