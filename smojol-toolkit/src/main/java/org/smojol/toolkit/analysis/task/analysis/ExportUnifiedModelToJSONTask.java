package org.smojol.toolkit.analysis.task.analysis;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.intermediate.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.graphml.SerialisableUnifiedModel;
import org.smojol.toolkit.analysis.pipeline.config.OutputArtifactConfig;
import com.mojo.algorithms.task.AnalysisTask;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.CommandLineAnalysisTask;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class ExportUnifiedModelToJSONTask implements AnalysisTask {
    private final UnifiedFlowModelTask unifiedFlowModelTask;
    private final OutputArtifactConfig unifiedModelOutputConfig;

    public ExportUnifiedModelToJSONTask(FlowNode flowRoot, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, OutputArtifactConfig unifiedModelOutputConfig, ResourceOperations resourceOperations) {
        unifiedFlowModelTask = new UnifiedFlowModelTask(flowRoot, dataStructures, qualifier);
        this.unifiedModelOutputConfig = unifiedModelOutputConfig;
    }

    public AnalysisTaskResult run() {
        SerialisableUnifiedModel unifiedModel = unifiedFlowModelTask.run();
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        Path parentDir = unifiedModelOutputConfig.outputDir();
        if (parentDir != null) {
            try {
                Files.createDirectories(parentDir);
            } catch (IOException e) {
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.EXPORT_UNIFIED_TO_JSON);
            }
        }
        try (JsonWriter writer = new JsonWriter(new FileWriter(unifiedModelOutputConfig.fullPath()))) {
            writer.setIndent("  ");
            gson.toJson(unifiedModel, SerialisableUnifiedModel.class, writer);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.EXPORT_UNIFIED_TO_JSON, unifiedModel);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.EXPORT_UNIFIED_TO_JSON);
        }
    }
}
