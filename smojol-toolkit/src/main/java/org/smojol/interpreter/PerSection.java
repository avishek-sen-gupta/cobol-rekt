package org.smojol.interpreter;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.analysis.ParsePipeline;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.navigation.CobolEntityNavigator;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

public class PerSection extends FlowchartGenerationStrategy {
    public PerSection(FlowchartOutputFormat outputFormat) {
        super(outputFormat);
    }

    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
        List<ParseTree> allSections = navigator.findAllByCondition(n -> n.getClass() == CobolParser.ProcedureSectionContext.class, root);
        for (ParseTree section : allSections) {
            pipeline.flowcharter().generateFlowchart(section,
                    FlowchartGenerationStrategy.outputPath(section, dotFileOutputDir, "dot"),
                    FlowchartGenerationStrategy.outputPath(section, imageOutputDir, this.outputFormat.extension()), this.outputFormat);
        }
    }
}

