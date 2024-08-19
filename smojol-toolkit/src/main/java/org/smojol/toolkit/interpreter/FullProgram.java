package org.smojol.toolkit.interpreter;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.toolkit.flowchart.FlowchartGenerationStrategy;

import java.io.IOException;
import java.nio.file.Path;

public class FullProgram extends FlowchartGenerationStrategy {
    public FullProgram(FlowchartOutputFormat outputFormat) {
        super(outputFormat);
    }

    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
        pipeline.flowcharter().generateFlowchart(root,
                FlowchartGenerationStrategy.outputPath(programName, dotFileOutputDir, "dot"),
                FlowchartGenerationStrategy.outputPath(programName, imageOutputDir, this.outputFormat.extension()), this.outputFormat);
    }
}
