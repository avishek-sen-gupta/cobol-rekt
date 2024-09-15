package org.smojol.toolkit.analysis.defined;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.analysis.pipeline.NodeOperationCostFunctions;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultError;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.pipeline.config.OutputArtifactConfig;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;

public class CompareCodeTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final CobolDataStructure dataStructures;
    private final NodeSpecBuilder qualifier;
    private final OutputArtifactConfig similarityOutputConfig;

    public CompareCodeTask(FlowNode astRoot, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, OutputArtifactConfig similarityOutputConfig, ResourceOperations resourceOperations) {
        this.astRoot = astRoot;
        this.dataStructures = dataStructures;
        this.qualifier = qualifier;
        this.similarityOutputConfig = similarityOutputConfig;
    }

    @Override
    public AnalysisTaskResult run() {
        AnalysisTaskResult result = new CompareCodeBlocksTask(dataStructures, qualifier).run(astRoot, NodeOperationCostFunctions.GENERIC);
        return switch (result) {
            case AnalysisTaskResultOK o -> exportToJSON(o.getDetail());
            case AnalysisTaskResultError e -> e;
        };
    }

    private AnalysisTaskResult exportToJSON(List<SimilarityResult> similarityResults) {
        List<SerialisableSimilarityResult> serialisableResults = similarityResults.stream().map(s ->
                new SerialisableSimilarityResult(s.nodes(), s.distance(), s.editOperationLists())).toList();
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        try {
            Files.createDirectories(similarityOutputConfig.outputDir());
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.COMPARE_CODE);
        }
        try (JsonWriter writer = new JsonWriter(new FileWriter(similarityOutputConfig.fullPath()))) {
            writer.setIndent("  ");
            gson.toJson(serialisableResults, List.class, writer);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.COMPARE_CODE);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.COMPARE_CODE);
        }
    }
}
