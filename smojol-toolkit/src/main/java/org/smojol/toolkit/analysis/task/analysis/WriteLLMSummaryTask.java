package org.smojol.toolkit.analysis.task.analysis;

import com.google.common.collect.ImmutableMap;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import com.mojo.woof.Advisor;
import com.mojo.woof.OpenAICredentials;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.SerialisableASTFlowNode;
import org.smojol.common.navigation.TreeMapperTraversal;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.pipeline.config.OutputArtifactConfig;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class WriteLLMSummaryTask implements AnalysisTask {
    private final FlowNode flowRoot;
    private final CobolDataStructure dataRoot;
    private final OutputArtifactConfig llmOutputConfig;
    private final ResourceOperations resourceOperations;

    public WriteLLMSummaryTask(FlowNode flowRoot, CobolDataStructure dataRoot, OutputArtifactConfig llmOutputConfig, ResourceOperations resourceOperations) {
        this.flowRoot = flowRoot;
        this.dataRoot = dataRoot;
        this.llmOutputConfig = llmOutputConfig;
        this.resourceOperations = resourceOperations;
    }

    @Override
    public AnalysisTaskResult run() {
        Map<String, SummaryTree> summaries = summariseThroughLLM(flowRoot, dataRoot);
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        try {
            resourceOperations.createDirectories(llmOutputConfig.outputDir());
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_LLM_SUMMARY);
        }
        try (JsonWriter writer = new JsonWriter(resourceOperations.fileWriter(llmOutputConfig.fullPath()))) {
            writer.setIndent("  ");
            gson.toJson(summaries, Map.class, writer);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_LLM_SUMMARY, summaries);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_LLM_SUMMARY);
        }
    }


    private static Map<String, SummaryTree> summariseThroughLLM(FlowNode flowRoot, CobolDataStructure dataRoot) {
        Advisor advisor = new Advisor(OpenAICredentials.fromEnv());
        Function<FlowNode, List<FlowNode>> codeChildrenFn = FlowNode::astChildren;
        Function<CobolDataStructure, List<CobolDataStructure>> dataChildrenFn = CobolDataStructure::subStructures;
        SummaryTree codeSummary = new TreeMapperTraversal<FlowNode, SummaryTree>().accept(flowRoot, new CodeSummaryVisitor(advisor), codeChildrenFn);
        SummaryTree dataSummary = new TreeMapperTraversal<CobolDataStructure, SummaryTree>().accept(dataRoot, new DataSummaryVisitor(advisor), dataChildrenFn);
        return ImmutableMap.of("codeSummary", codeSummary, "dataSummary", dataSummary);
    }
}
