package org.smojol.toolkit.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class FlowchartOutputWriter {
    private final FlowchartGenerationStrategy flowchartGenerationStrategy;
    private final Path dotFileOutputDir;
    private final Path imageOutputDir;

    public FlowchartOutputWriter(FlowchartGenerationStrategy flowchartGenerationStrategy, Path dotFileOutputDir, Path imageOutputDir) {
        this.flowchartGenerationStrategy = flowchartGenerationStrategy;
        this.dotFileOutputDir = dotFileOutputDir;
        this.imageOutputDir = imageOutputDir;
    }

    public void draw(CobolEntityNavigator navigator, ParseTree root, SourceConfig sourceConfig, FlowchartBuilder flowcharter) throws IOException, InterruptedException {
        flowchartGenerationStrategy.draw(navigator, root, flowcharter, dotFileOutputDir, imageOutputDir, sourceConfig.programName());
    }

    public void createOutputDirs() throws IOException {
        Files.createDirectories(dotFileOutputDir);
        Files.createDirectories(imageOutputDir);
    }
}
