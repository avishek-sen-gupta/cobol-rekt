package org.smojol.toolkit.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.toolkit.interpreter.FullProgram;

import java.io.IOException;
import java.nio.file.Path;
import java.util.logging.Logger;

import static org.smojol.common.flowchart.FlowchartOutputFormat.PNG;
import static org.smojol.common.flowchart.FlowchartOutputFormat.SVG;

public abstract class FlowchartGenerationStrategy {
    java.util.logging.Logger LOGGER = Logger.getLogger(FlowchartGenerationStrategy.class.getName());
    protected final FlowchartOutputFormat outputFormat;
    public static final FlowchartGenerationStrategy DONT_DRAW = new FlowchartGenerationStrategy(null) {
        @Override
        public void draw(CobolEntityNavigator navigator, ParseTree root, FlowchartBuilder flowcharter, Path dotFileOutputDir, Path imageOutputDir, String programName) {
            LOGGER.info("Not Drawing Flowchart...");
        }
    };

    public FlowchartGenerationStrategy(FlowchartOutputFormat outputFormat) {
        this.outputFormat = outputFormat;
    }

    public abstract void draw(CobolEntityNavigator navigator, ParseTree root, FlowchartBuilder flowcharter, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException;

    public static FlowchartGenerationStrategy strategy(String generationStrategyAsString, String flowchartOutputFormatAsString) {
        FlowchartOutputFormat flowchartOutputFormat = "PNG".equals(flowchartOutputFormatAsString) ? PNG : SVG;
        if (generationStrategyAsString == null) return new FullProgram(flowchartOutputFormat);
        return switch (generationStrategyAsString) {
            case "SECTION" -> new PerSection(flowchartOutputFormat);
            case "PARAGRAPH" -> new PerParagraph(flowchartOutputFormat);
            case "PROGRAM" -> new FullProgram(flowchartOutputFormat);
            case "NODRAW" -> DONT_DRAW;
            default -> DONT_DRAW;
        };
    }

    protected static String outputPath(String label, Path outputDir, String extension) {
        return outputDir.resolve(String.format("%s.%s", label, extension)).toString();
    }

    static String outputPath(CobolParser.ProcedureSectionContext section, Path outputDir, String extension) {
        String sectionName = section.procedureSectionHeader().sectionName().getText();
        return outputPath(sectionName, outputDir, extension);
    }

    static String outputPath(CobolParser.ParagraphContext paragraph, Path outputDir, String extension) {
        String paragraphName = paragraph.paragraphDefinitionName().getText();
        return outputPath(paragraphName, outputDir, extension);
    }
}
