package org.smojol.interpreter;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.analysis.ParsePipeline;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import static org.smojol.flowchart.FlowchartTasks.outputPath;

public interface FlowchartGenerationStrategy {
    FlowchartGenerationStrategy PER_SECTION = new PerSection();
    FlowchartGenerationStrategy FULL_PROGRAM = new FullProgram();
    FlowchartGenerationStrategy DONT_DRAW = new DontDraw();

    static FlowchartGenerationStrategy strategy(String generationStrategyAsString) {
        if (generationStrategyAsString == null) return FULL_PROGRAM;
        return switch (generationStrategyAsString) {
            case "SECTION" -> PER_SECTION;
            case "PROGRAM" -> FULL_PROGRAM;
            case "NODRAW" -> DONT_DRAW;
            default -> DONT_DRAW;
        };
    }

    void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException;
}

class DontDraw implements FlowchartGenerationStrategy {
    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
    }
}

class PerSection implements FlowchartGenerationStrategy {
    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
        List<ParseTree> allSections = navigator.findAllByCondition(n -> n.getClass() == CobolParser.ProcedureSectionContext.class, root);
        for (ParseTree section : allSections) {
            pipeline.flowcharter().generateFlowchart(section,
                    outputPath(section, dotFileOutputDir, "dot"),
                    outputPath(section, imageOutputDir, "png"), "ortho");
        }
    }
}

class FullProgram implements FlowchartGenerationStrategy {
    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
        pipeline.flowcharter().generateFlowchart(root,
                outputPath(programName, dotFileOutputDir, "dot"),
                outputPath(programName, imageOutputDir, "png"), "ortho");
    }
}
