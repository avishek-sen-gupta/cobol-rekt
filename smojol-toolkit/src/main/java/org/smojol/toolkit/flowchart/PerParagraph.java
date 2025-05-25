package org.smojol.toolkit.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import com.mojo.algorithms.visualisation.FlowchartOutputFormat;
import com.mojo.algorithms.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.common.vm.structure.Format1DataStructure;
import org.smojol.toolkit.ast.BuildFlowNodesTask;
import org.smojol.toolkit.ast.FlowNodeServiceImpl;
import org.smojol.toolkit.ast.FlowchartBuilder;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.logging.Logger;

public class PerParagraph extends FlowchartGenerationStrategy {
    java.util.logging.Logger LOGGER = Logger.getLogger(PerParagraph.class.getName());
    private final IdProvider idProvider;

    public PerParagraph(FlowchartOutputFormat outputFormat, IdProvider idProvider) {
        super(outputFormat);
        this.idProvider = idProvider;
    }

    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
        List<CobolParser.ParagraphContext> allParagraphs = navigator.findAllByCondition(n -> n.getClass() == CobolParser.ParagraphContext.class, root).stream().map(p -> (CobolParser.ParagraphContext) p).toList();
        for (CobolParser.ParagraphContext paragraph : allParagraphs) {
            FlowNodeService nodeService = new FlowNodeServiceImpl(new CobolEntityNavigator(paragraph),
                    new Format1DataStructure(0, new UnresolvedReferenceDoNothingStrategy()),
                    idProvider);
            FlowNode flowParagraph = new BuildFlowNodesTask(nodeService).run(paragraph);
            LOGGER.info("Generating flowchart for paragraph: " + paragraph.paragraphDefinitionName().getText());
            new FlowchartBuilder(flowParagraph).build(
                    FlowchartGenerationStrategy.outputPath(paragraph, dotFileOutputDir, "dot"),
                    FlowchartGenerationStrategy.outputPath(paragraph, imageOutputDir, this.outputFormat.extension()), this.outputFormat);
        }
    }
}
