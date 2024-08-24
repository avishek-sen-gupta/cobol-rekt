package org.smojol.toolkit.analysis.defined;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.id.IdProvider;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.analysis.pipeline.SerialisableCFGGraphCollector;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.analysis.pipeline.config.CFGOutputConfig;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;

public class WriteControlFlowGraphTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final IdProvider idProvider;
    private final CFGOutputConfig cfgOutputConfig;

    public WriteControlFlowGraphTask(FlowNode astRoot, IdProvider idProvider, CFGOutputConfig cfgOutputConfig) {
        this.astRoot = astRoot;
        this.idProvider = idProvider;
        this.cfgOutputConfig = cfgOutputConfig;
    }

    @Override
    public AnalysisTaskResult run() {
        SerialisableCFGGraphCollector cfgGraphCollector = new SerialisableCFGGraphCollector(idProvider);
        astRoot.accept(cfgGraphCollector, -1);
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        try {
            Files.createDirectories(cfgOutputConfig.outputDir());
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_CFG);
        }
        try (JsonWriter writer = new JsonWriter(new FileWriter(cfgOutputConfig.outputPath()))) {
            writer.setIndent("  ");  // Optional: for pretty printing
            gson.toJson(cfgGraphCollector, SerialisableCFGGraphCollector.class, writer);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_CFG);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_CFG);
        }
    }
}
