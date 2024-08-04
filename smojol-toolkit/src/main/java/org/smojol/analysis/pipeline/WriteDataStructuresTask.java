package org.smojol.analysis.pipeline;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.analysis.pipeline.config.OutputArtifactConfig;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;

public class WriteDataStructuresTask implements Runnable {
    private final CobolDataStructure dataStructures;
    private final OutputArtifactConfig outputArtifactConfig;

    public WriteDataStructuresTask(CobolDataStructure dataStructures, OutputArtifactConfig outputArtifactConfig) {
        this.dataStructures = dataStructures;
        this.outputArtifactConfig = outputArtifactConfig;
    }

    @Override
    public void run() {
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        SerialisableCobolDataStructure root = new SerialisableCobolDataStructure();
        DataStructureExporter visitor = new DataStructureExporter(root);
        dataStructures.acceptStrange(visitor);
        String json = gson.toJson(root);
        System.out.println("Output: " + outputArtifactConfig.fullPath());
        try {
            Files.createDirectory(outputArtifactConfig.outputDir());
            PrintWriter out = new PrintWriter(outputArtifactConfig.fullPath());
            out.println(json);
            out.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}