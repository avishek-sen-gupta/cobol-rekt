package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeType;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.toolkit.analysis.pipeline.config.OutputArtifactConfig;
import org.smojol.toolkit.flowchart.MermainChartBuilder;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

public class ExportMermaidTask implements AnalysisTask {
    private final FlowNode flowRoot;
    private final OutputArtifactConfig mermaidOutputConfig;

    public ExportMermaidTask(FlowNode flowRoot, OutputArtifactConfig mermaidOutputConfig) {
        this.flowRoot = flowRoot;
        this.mermaidOutputConfig = mermaidOutputConfig;
    }

    @Override
    public AnalysisTaskResult run() {
        List<FlowNode> sections = new FlowNodeNavigator(flowRoot).findAllByCondition(fn -> fn.type() == FlowNodeType.SECTION);
        try {
            writeSections(sections);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.EXPORT_MERMAID);
        } catch (Exception e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.EXPORT_MERMAID);
        }
    }

    private void writeSections(List<FlowNode> sections) throws IOException {
        Files.createDirectories(mermaidOutputConfig.outputDir());
        sections.forEach(s -> {
            List<String> mermaidGraph = new MermainChartBuilder().build(s);
            String sectionOutputPath = Paths.get(mermaidOutputConfig.outputDir().toString(), s.name() + ".md").toString();
            try (PrintWriter writer = new PrintWriter(sectionOutputPath, StandardCharsets.UTF_8)){
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
        });
    }
}
