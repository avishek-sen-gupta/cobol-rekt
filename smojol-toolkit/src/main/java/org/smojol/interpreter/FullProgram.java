package org.smojol.interpreter;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.analysis.ParsePipeline;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.navigation.CobolEntityNavigator;

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
