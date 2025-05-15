package org.smojol.toolkit.interpreter;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import com.mojo.algorithms.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.common.vm.structure.Format1DataStructure;
import org.smojol.toolkit.ast.BuildFlowNodesTask;
import org.smojol.toolkit.ast.FlowNodeServiceImpl;
import org.smojol.toolkit.ast.FlowchartBuilder;
import org.smojol.toolkit.flowchart.FlowchartGenerationStrategy;

import java.io.IOException;
import java.nio.file.Path;

public class FullProgram extends FlowchartGenerationStrategy {
    private final IdProvider idProvider;

    public FullProgram(FlowchartOutputFormat outputFormat, IdProvider idProvider) {
        super(outputFormat);
        this.idProvider = idProvider;
    }

    @Override
    public void draw(CobolEntityNavigator navigator, ParseTree root, Path dotFileOutputDir, Path imageOutputDir, String programName) throws IOException, InterruptedException {
        FlowNodeService nodeService = new FlowNodeServiceImpl(new CobolEntityNavigator(root),
                new Format1DataStructure(0, new UnresolvedReferenceDoNothingStrategy()),
                idProvider);
        FlowNode flowRoot = new BuildFlowNodesTask(nodeService).run(root);
        new FlowchartBuilder(flowRoot).build(
                FlowchartGenerationStrategy.outputPath(programName, dotFileOutputDir, "dot"),
                FlowchartGenerationStrategy.outputPath(programName, imageOutputDir, this.outputFormat.extension()), this.outputFormat);
    }
}
