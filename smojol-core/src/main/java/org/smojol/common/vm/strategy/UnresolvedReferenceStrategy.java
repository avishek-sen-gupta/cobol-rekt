package org.smojol.common.vm.strategy;

import java.util.List;
import org.smojol.common.vm.structure.CobolDataStructure;

public interface UnresolvedReferenceStrategy {
  void runIfResolved(List<? extends CobolDataStructure> path, String recordID, Runnable r);
}
