package org.smojol.toolkit.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import com.mojo.algorithms.visualisation.FlowchartOutputFormat;
import com.mojo.algorithms.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.SectionNameExtractor;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.common.vm.structure.Format1DataStructure;
import org.smojol.toolkit.ast.BuildFlowNodesTask;
import org.smojol.toolkit.ast.FlowNodeServiceImpl;
import org.smojol.toolkit.ast.FlowchartBuilder;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.logging.Logger;

public class PerSection extends FlowchartGenerationStrategy {
    java.util.logging.Logger LOGGER = Logger.getLogger(PerSection.class.getName());
    private final IdProvider idProvider;

    public PerSection(FlowchartOutputFormat outputFormat, IdProvider idProvider) {
        super(outputFormat);
        this.idProvider = idProvider;
    }

    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
        List<CobolParser.SectionOrParagraphContext> allSections = navigator.findAllByCondition(n -> n.getClass() == CobolParser.SectionOrParagraphContext.class && ((CobolParser.SectionOrParagraphContext) n).SECTION() != null, root).stream().map(s -> (CobolParser.SectionOrParagraphContext) s).toList();
        for (CobolParser.SectionOrParagraphContext section : allSections) {
            FlowNodeService nodeService = new FlowNodeServiceImpl(new CobolEntityNavigator(section),
                    new Format1DataStructure(0, new UnresolvedReferenceDoNothingStrategy()),
                    idProvider);
            FlowNode flowSection = new BuildFlowNodesTask(nodeService).run(section);
            String sectionLabel = new SectionNameExtractor().sectionName(section);
            LOGGER.info("Generating flowchart for section: " + sectionLabel);
            new FlowchartBuilder(flowSection).build(
                    FlowchartGenerationStrategy.outputPath(section, dotFileOutputDir, "dot"),
                    FlowchartGenerationStrategy.outputPath(section, imageOutputDir, this.outputFormat.extension()), this.outputFormat);
        }
    }
}

