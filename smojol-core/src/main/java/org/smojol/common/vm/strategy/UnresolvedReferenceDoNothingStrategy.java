package org.smojol.common.vm.strategy;

import java.util.List;
import java.util.logging.Logger;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.vm.structure.CobolDataStructure;

public class UnresolvedReferenceDoNothingStrategy implements UnresolvedReferenceStrategy {
  private static final java.util.logging.Logger LOGGER =
      Logger.getLogger(UnresolvedReferenceDoNothingStrategy.class.getName());

  @Override
  public void runIfResolved(List<? extends CobolDataStructure> path, String recordID, Runnable r) {
    if (path.isEmpty()) {
      LOGGER.warning(
          ConsoleColors.red(
              String.format("Record with ID %s was not found, skipping set()", recordID)));
      return;
    }
    r.run();
  }
}
