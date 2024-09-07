package org.smojol.toolkit.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.toolkit.analysis.defined.ProgramDependenciesTask;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.logging.Logger;

public class PerSection extends FlowchartGenerationStrategy {
    java.util.logging.Logger LOGGER = Logger.getLogger(PerSection.class.getName());
    public PerSection(FlowchartOutputFormat outputFormat) {
        super(outputFormat);
    }

    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, FlowchartBuilder flowcharter, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
        List<CobolParser.ProcedureSectionContext> allSections = navigator.findAllByCondition(n -> n.getClass() == CobolParser.ProcedureSectionContext.class, root).stream().map(s -> (CobolParser.ProcedureSectionContext) s).toList();
        for (CobolParser.ProcedureSectionContext section : allSections) {
            LOGGER.info("Generating flowchart for section: " + section.procedureSectionHeader().sectionName());
            flowcharter.generateFlowchart(section,
                    FlowchartGenerationStrategy.outputPath(section, dotFileOutputDir, "dot"),
                    FlowchartGenerationStrategy.outputPath(section, imageOutputDir, this.outputFormat.extension()), this.outputFormat);
        }
    }
}

