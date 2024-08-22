package org.smojol.toolkit.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

public class PerParagraph extends FlowchartGenerationStrategy {
    public PerParagraph(FlowchartOutputFormat outputFormat) {
        super(outputFormat);
    }

    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, ParsePipeline pipeline, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
        List<CobolParser.ParagraphContext> allParagraphs = navigator.findAllByCondition(n -> n.getClass() == CobolParser.ParagraphContext.class, root).stream().map(p -> (CobolParser.ParagraphContext) p).toList();
        for (CobolParser.ParagraphContext paragraph : allParagraphs) {
            System.out.println("Generating flowchart for paragraph: " + paragraph.paragraphDefinitionName().getText());
            pipeline.flowcharter().generateFlowchart(paragraph,
                    FlowchartGenerationStrategy.outputPath(paragraph, dotFileOutputDir, "dot"),
                    FlowchartGenerationStrategy.outputPath(paragraph, imageOutputDir, this.outputFormat.extension()), this.outputFormat);
        }
    }
}

