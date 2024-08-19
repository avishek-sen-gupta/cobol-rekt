package org.smojol.toolkit.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.nio.file.Path;

class DontDraw extends FlowchartGenerationStrategy {
    public DontDraw() {
        super(null);
    }

    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) {
    }
}
