package org.smojol.interpreter;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.analysis.ParsePipeline;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

public interface FlowchartGenerationStrategy {
    FlowchartGenerationStrategy PER_SECTION = new PerSection();
    FlowchartGenerationStrategy FULL_PROGRAM = new FullProgram();
    FlowchartGenerationStrategy DONT_DRAW = new DontDraw();

    void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException;

    static FlowchartGenerationStrategy strategy(String generationStrategyAsString) {
        if (generationStrategyAsString == null) return FULL_PROGRAM;
        return switch (generationStrategyAsString) {
            case "SECTION" -> PER_SECTION;
            case "PROGRAM" -> FULL_PROGRAM;
            case "NODRAW" -> DONT_DRAW;
            default -> DONT_DRAW;
        };
    }

    static String outputPath(String label, Path outputDir, String extension) {
        return outputDir.resolve(String.format("%s.%s", label, extension)).toString();
    }

    static String outputPath(ParseTree section, Path outputDir, String extension) {
        CobolParser.ProcedureSectionContext s = (CobolParser.ProcedureSectionContext) section;
        String sectionName = s.procedureSectionHeader().sectionName().getText();
        return outputPath(sectionName, outputDir, extension);
    }
}

class DontDraw implements FlowchartGenerationStrategy {
    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) {
        System.out.println("Not Drawing Flowchart...");
    }
}

class PerSection implements FlowchartGenerationStrategy {
    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
        List<ParseTree> allSections = navigator.findAllByCondition(n -> n.getClass() == CobolParser.ProcedureSectionContext.class, root);
        for (ParseTree section : allSections) {
            pipeline.flowcharter().generateFlowchart(section,
                    FlowchartGenerationStrategy.outputPath(section, dotFileOutputDir, "dot"),
                    FlowchartGenerationStrategy.outputPath(section, imageOutputDir, "png"), "ortho");
        }
    }
}

class FullProgram implements FlowchartGenerationStrategy {
    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
        pipeline.flowcharter().generateFlowchart(root,
                FlowchartGenerationStrategy.outputPath(programName, dotFileOutputDir, "dot"),
                FlowchartGenerationStrategy.outputPath(programName, imageOutputDir, "png"), "ortho");
    }
}
