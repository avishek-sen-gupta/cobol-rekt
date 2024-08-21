package org.smojol.toolkit.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.toolkit.interpreter.FullProgram;

import java.io.IOException;
import java.nio.file.Path;

import static org.smojol.common.flowchart.FlowchartOutputFormat.PNG;
import static org.smojol.common.flowchart.FlowchartOutputFormat.SVG;

public abstract class FlowchartGenerationStrategy {
    protected final FlowchartOutputFormat outputFormat;
    public static final FlowchartGenerationStrategy DONT_DRAW = new FlowchartGenerationStrategy(null) {
        @Override
        public void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) {
            System.out.println("Not Drawing Flowchart...");
        }
    };

    public FlowchartGenerationStrategy(FlowchartOutputFormat outputFormat) {
        this.outputFormat = outputFormat;
    }

    public abstract void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException;

    public static FlowchartGenerationStrategy strategy(String generationStrategyAsString, String flowchartOutputFormatAsString) {
        FlowchartOutputFormat flowchartOutputFormat = "PNG".equals(flowchartOutputFormatAsString) ? PNG : SVG;
        if (generationStrategyAsString == null) return new FullProgram(flowchartOutputFormat);
        return switch (generationStrategyAsString) {
            case "SECTION" -> new PerSection(flowchartOutputFormat);
            case "PROGRAM" -> new FullProgram(flowchartOutputFormat);
            case "NODRAW" -> DONT_DRAW;
            default -> DONT_DRAW;
        };
    }

    protected static String outputPath(String label, Path outputDir, String extension) {
        return outputDir.resolve(String.format("%s.%s", label, extension)).toString();
    }

    static String outputPath(ParseTree section, Path outputDir, String extension) {
        CobolParser.ProcedureSectionContext s = (CobolParser.ProcedureSectionContext) section;
        String sectionName = s.procedureSectionHeader().sectionName().getText();
        return outputPath(sectionName, outputDir, extension);
    }
}