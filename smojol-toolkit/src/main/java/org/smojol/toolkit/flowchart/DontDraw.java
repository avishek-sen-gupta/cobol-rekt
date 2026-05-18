package org.smojol.toolkit.flowchart;

import java.nio.file.Path;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.navigation.CobolEntityNavigator;

class DontDraw extends FlowchartGenerationStrategy {
  public DontDraw() {
    super(null);
  }

  @Override
  public void draw(
      CobolEntityNavigator navigator,
      ParseTree root,
      Path dotFileOutputDir,
      Path imageOutputDir,
      String programName) {}
}
