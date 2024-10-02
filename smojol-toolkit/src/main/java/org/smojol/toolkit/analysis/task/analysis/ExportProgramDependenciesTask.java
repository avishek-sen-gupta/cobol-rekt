package org.smojol.toolkit.analysis.task.analysis;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import org.smojol.common.program.CobolProgram;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;

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
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES);
            }
        }
        try (JsonWriter writer = new JsonWriter(new FileWriter(exportPath))) {
            writer.setIndent("  ");
            gson.toJson(root, CobolProgram.class, writer);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES);
        }
    }
}
