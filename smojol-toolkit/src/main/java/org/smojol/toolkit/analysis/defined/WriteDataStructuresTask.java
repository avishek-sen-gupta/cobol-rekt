package org.smojol.toolkit.analysis.defined;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.analysis.pipeline.DataStructureExporter;
import org.smojol.toolkit.analysis.pipeline.SerialisableCobolDataStructure;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.pipeline.config.OutputArtifactConfig;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;

public class WriteDataStructuresTask implements AnalysisTask {
    private final CobolDataStructure dataStructures;
    private final OutputArtifactConfig outputArtifactConfig;

    public WriteDataStructuresTask(CobolDataStructure dataStructures, OutputArtifactConfig outputArtifactConfig, ResourceOperations resourceOperations) {
        this.dataStructures = dataStructures;
        this.outputArtifactConfig = outputArtifactConfig;
    }

    @Override
    public AnalysisTaskResult run() {
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        SerialisableCobolDataStructure root = new SerialisableCobolDataStructure();
        DataStructureExporter visitor = new DataStructureExporter(root);
        dataStructures.acceptScopedVisitor(visitor);
        SerialisableCobolDataStructure realRoot = root.getChild(0);
        try {
            Files.createDirectories(outputArtifactConfig.outputDir());
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_DATA_STRUCTURES);
        }
        try (JsonWriter writer = new JsonWriter(new FileWriter(outputArtifactConfig.fullPath()))) {
            writer.setIndent("  ");
            gson.toJson(realRoot, SerialisableCobolDataStructure.class, writer);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_DATA_STRUCTURES);
        }
        return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_DATA_STRUCTURES, realRoot);
    }
}
