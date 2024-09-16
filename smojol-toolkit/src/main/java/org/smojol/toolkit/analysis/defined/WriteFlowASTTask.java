package org.smojol.toolkit.analysis.defined;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.SerialisableASTFlowNode;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.analysis.pipeline.config.FlowASTOutputConfig;

import java.io.IOException;

public class WriteFlowASTTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final FlowASTOutputConfig flowASTOutputConfig;
    private final ResourceOperations resourceOperations;

    public WriteFlowASTTask(FlowNode astRoot, FlowASTOutputConfig flowASTOutputConfig, ResourceOperations resourceOperations) {
        this.astRoot = astRoot;
        this.flowASTOutputConfig = flowASTOutputConfig;
        this.resourceOperations = resourceOperations;
    }

    @Override
    public AnalysisTaskResult run() {
        SerialisableASTFlowNode serialisableASTFlowRoot = new SerialiseFlowASTTask().serialisedFlowAST(astRoot);
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        try {
            resourceOperations.createDirectories(flowASTOutputConfig.outputDir());
//            Files.createDirectories(flowASTOutputConfig.outputDir());
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_FLOW_AST);
        }
        try (JsonWriter writer = new JsonWriter(resourceOperations.fileWriter(flowASTOutputConfig.outputPath()))) {
//        try (JsonWriter writer = new JsonWriter(new FileWriter(flowASTOutputConfig.outputPath()))) {
            writer.setIndent("  ");
            gson.toJson(serialisableASTFlowRoot, SerialisableASTFlowNode.class, writer);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_FLOW_AST, serialisableASTFlowRoot);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_FLOW_AST);
        }
    }
}
