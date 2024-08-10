package org.smojol.analysis.pipeline;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import com.mojo.woof.GraphSDK;
import org.smojol.analysis.visualisation.CobolProgram;
import org.smojol.common.ast.SerialisableASTFlowNode;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class ExportProgramDependenciesTask {
    private final String exportPath;

    public ExportProgramDependenciesTask(String exportPath) {
        this.exportPath = exportPath;
    }

    public AnalysisTaskResult run(CobolProgram root) {
        Gson gson = new GsonBuilder().setPrettyPrinting().excludeFieldsWithoutExposeAnnotation().create();
        Path path = Paths.get(exportPath);
        Path parentDir = path.getParent();
        if (parentDir != null) {
            try {
                Files.createDirectories(parentDir);
            } catch (IOException e) {
                return AnalysisTaskResult.ERROR(e);
            }
        }
        try (JsonWriter writer = new JsonWriter(new FileWriter(exportPath))) {
            writer.setIndent("  ");
            gson.toJson(root, SerialisableASTFlowNode.class, writer);
            return AnalysisTaskResult.OK();
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e);
        }
    }
}
