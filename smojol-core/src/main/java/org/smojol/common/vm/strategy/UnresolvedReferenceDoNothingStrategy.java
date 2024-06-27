package org.smojol.common.vm.strategy;

import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.vm.structure.CobolDataStructure;

import java.util.List;

public class UnresolvedReferenceDoNothingStrategy implements UnresolvedReferenceStrategy {
    @Override
    public void runIfResolved(List<? extends CobolDataStructure> path, String recordID, Runnable r) {
        if (path.isEmpty()) {
            System.out.println(ConsoleColors.red(String.format("WARNING: Record with ID %s was not found, skipping set()", recordID)));
            return;
        }
        r.run();
    }
}
