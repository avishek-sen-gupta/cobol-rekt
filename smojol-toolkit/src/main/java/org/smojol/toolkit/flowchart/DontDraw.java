package org.smojol.toolkit.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.nio.file.Path;

class DontDraw extends FlowchartGenerationStrategy {
    public DontDraw() {
        super(null);
    }

    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, FlowchartBuilder flowcharter, Path dotFileOutputDir, Path imageOutputDir, String programName) {
    }
}
