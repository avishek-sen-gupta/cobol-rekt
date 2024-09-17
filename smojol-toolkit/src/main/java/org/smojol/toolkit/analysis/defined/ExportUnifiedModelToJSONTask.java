package org.smojol.toolkit.analysis.defined;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeSymbolExtractorVisitor;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.pseudocode.SymbolReferenceBuilder;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.common.ast.FlowNodeASTTraversal;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.graphml.JGraphTGraphBuilder;
import org.smojol.toolkit.analysis.graph.graphml.SerialisableUnifiedModel;
import org.smojol.toolkit.analysis.pipeline.config.OutputArtifactConfig;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class ExportUnifiedModelToJSONTask implements AnalysisTask {
    private final FlowNode flowRoot;
    private final CobolDataStructure dataStructures;
    private final NodeSpecBuilder qualifier;
    private final OutputArtifactConfig unifiedModelOutputConfig;

    public ExportUnifiedModelToJSONTask(FlowNode flowRoot, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, OutputArtifactConfig unifiedModelOutputConfig, ResourceOperations resourceOperations) {
        this.flowRoot = flowRoot;
        this.dataStructures = dataStructures;
        this.qualifier = qualifier;
        this.unifiedModelOutputConfig = unifiedModelOutputConfig;
    }

    public AnalysisTaskResult run() {
//        new FlowNodeASTTraversal<FlowNode>().accept(flowRoot, new FlowNodeSymbolExtractorVisitor(flowRoot, dataStructures, new SmojolSymbolTable(dataStructures, new SymbolReferenceBuilder(new UUIDProvider()))));
        JGraphTGraphBuilder graphMLExporter = new JGraphTGraphBuilder(dataStructures, flowRoot, qualifier);
        graphMLExporter.buildAST();
        graphMLExporter.buildCFG();
        graphMLExporter.buildDataStructures();
        SerialisableUnifiedModel unifiedModel = graphMLExporter.asSerialisable();
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
