package org.smojol.toolkit.analysis.defined;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.toolkit.flowchart.FlowchartOutputWriter;

import java.io.IOException;

public class DrawFlowchartTask implements AnalysisTask {
    private final SourceConfig sourceConfig;
    private final FlowchartOutputWriter flowchartOutputWriter;
    private final CobolEntityNavigator navigator;
    private final FlowchartBuilder flowcharter;

    public DrawFlowchartTask(FlowchartBuilder flowcharter, CobolEntityNavigator navigator, FlowchartOutputWriter flowchartOutputWriter, SourceConfig sourceConfig, ResourceOperations resourceOperations) {
        this.sourceConfig = sourceConfig;
        this.flowchartOutputWriter = flowchartOutputWriter;
        this.navigator = navigator;
        this.flowcharter = flowcharter;
    }

    @Override
    public AnalysisTaskResult run() {
        ParseTree root = navigator.procedureBodyRoot();
        try {
            flowchartOutputWriter.createOutputDirs();
            flowchartOutputWriter.draw(navigator, root, sourceConfig, flowcharter);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.DRAW_FLOWCHART);
        } catch (IOException | InterruptedException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.DRAW_FLOWCHART);
        }
    }
}
