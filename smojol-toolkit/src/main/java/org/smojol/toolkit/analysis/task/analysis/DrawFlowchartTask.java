package org.smojol.toolkit.analysis.task.analysis;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.resource.ResourceOperations;
import com.mojo.algorithms.task.CommandLineAnalysisTask;
import com.mojo.algorithms.task.AnalysisTask;
import com.mojo.algorithms.task.AnalysisTaskResult;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.toolkit.flowchart.FlowchartOutputWriter;

import java.io.IOException;

public class DrawFlowchartTask implements AnalysisTask {
    private final SourceConfig sourceConfig;
    private final FlowchartOutputWriter flowchartOutputWriter;
    private final CobolEntityNavigator navigator;

    public DrawFlowchartTask(CobolEntityNavigator navigator, FlowchartOutputWriter flowchartOutputWriter, SourceConfig sourceConfig, ResourceOperations resourceOperations, FlowNode flowRoot) {
        this.sourceConfig = sourceConfig;
        this.flowchartOutputWriter = flowchartOutputWriter;
        this.navigator = navigator;
    }

    @Override
    public AnalysisTaskResult run() {
        ParseTree root = navigator.procedureDivisionBody(navigator.getRoot());
        try {

            flowchartOutputWriter.createOutputDirs();
            flowchartOutputWriter.draw(navigator, root, sourceConfig);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.DRAW_FLOWCHART);
        } catch (IOException | InterruptedException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.DRAW_FLOWCHART);
        }
    }
}
