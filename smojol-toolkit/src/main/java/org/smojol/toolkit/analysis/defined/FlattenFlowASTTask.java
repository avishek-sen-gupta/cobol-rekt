package org.smojol.toolkit.analysis.defined;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.SerialisableASTFlowNode;
import org.smojol.toolkit.analysis.pipeline.config.FlowASTOutputConfig;
import org.smojol.toolkit.interpreter.navigation.FlowNodeASTTraversal;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;

public class FlattenFlowASTTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final FlowASTOutputConfig flowASTOutputConfig;

    public FlattenFlowASTTask(FlowNode astRoot, FlowASTOutputConfig flowASTOutputConfig) {
        this.astRoot = astRoot;
        this.flowASTOutputConfig = flowASTOutputConfig;
    }

    @Override
    public AnalysisTaskResult run() {
        new BuildPseudocodeTask(astRoot).run();
        SerialisableASTFlowNode serialisableASTFlowRoot = new SerialiseFlowASTTask().serialisedFlowAST(astRoot);
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        try {
            Files.createDirectories(flowASTOutputConfig.outputDir());
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_FLOW_AST);
        }
        try (JsonWriter writer = new JsonWriter(new FileWriter(flowASTOutputConfig.outputPath()))) {
            writer.setIndent("  ");
            gson.toJson(serialisableASTFlowRoot, SerialisableASTFlowNode.class, writer);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_FLOW_AST);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_FLOW_AST);
        }
    }
}
