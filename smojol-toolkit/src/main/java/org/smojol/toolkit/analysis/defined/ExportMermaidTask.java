package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.toolkit.analysis.pipeline.config.OutputArtifactConfig;
import org.smojol.toolkit.flowchart.MermainChartBuilder;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Paths;
import java.util.List;

public class ExportMermaidTask implements AnalysisTask {
    private final FlowNode flowRoot;
    private final OutputArtifactConfig mermaidOutputConfig;
    private final ResourceOperations resourceOperations;

    public ExportMermaidTask(FlowNode flowRoot, OutputArtifactConfig mermaidOutputConfig, ResourceOperations resourceOperations) {
        this.flowRoot = flowRoot;
        this.mermaidOutputConfig = mermaidOutputConfig;
        this.resourceOperations = resourceOperations;
    }

    @Override
    public AnalysisTaskResult run() {
        List<FlowNode> sections = new FlowNodeNavigator(flowRoot).findAllByCondition(fn -> fn.type() == FlowNodeType.SECTION);
        try {
            resourceOperations.createDirectories(mermaidOutputConfig.outputDir());
//            Files.createDirectories(mermaidOutputConfig.outputDir());
//            writeCodeBlock(flowRoot);
            sections.forEach(this::writeCodeBlock);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.EXPORT_MERMAID);
        } catch (Exception e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.EXPORT_MERMAID);
        }
    }

    private void writeCodeBlock(FlowNode s) {
        String filename = s.type() == FlowNodeType.PROCEDURE_DIVISION_BODY ? s.id() : s.name();
        List<String> mermaidGraph = new MermainChartBuilder().build(s);
        String sectionOutputPath = Paths.get(mermaidOutputConfig.outputDir().toString(), filename + ".md").toString();
//        try (PrintWriter writer = new PrintWriter(sectionOutputPath, StandardCharsets.UTF_8)){
        try (PrintWriter writer = resourceOperations.printWriter(sectionOutputPath)){
            writer.println("---");
            writer.println("title: Section " + s.name());
            writer.println("---");
            writer.println("```mermaid");
            writer.println("flowchart TD");
            mermaidGraph.forEach(writer::println);
            writer.println("```");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
