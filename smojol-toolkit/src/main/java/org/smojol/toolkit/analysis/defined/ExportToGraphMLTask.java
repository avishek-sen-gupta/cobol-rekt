package org.smojol.toolkit.analysis.defined;

import org.jgrapht.Graph;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeSymbolExtractorVisitor;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphEdge;
import org.smojol.toolkit.analysis.graph.graphml.TypedGraphVertex;
import org.smojol.common.ast.FlowNodeASTTraversal;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.graphml.JGraphTGraphBuilder;
import org.smojol.toolkit.analysis.pipeline.config.GraphMLExportConfig;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

public class ExportToGraphMLTask implements AnalysisTask {
    private final FlowNode astRoot;
    private final CobolDataStructure dataStructures;
    private final GraphMLExportConfig graphMLOutputConfig;
    private final NodeSpecBuilder qualifier;

    public ExportToGraphMLTask(FlowNode astRoot, CobolDataStructure dataStructures, GraphMLExportConfig graphMLOutputConfig, NodeSpecBuilder qualifier, ResourceOperations resourceOperations) {
        this.astRoot = astRoot;
        this.dataStructures = dataStructures;
        this.graphMLOutputConfig = graphMLOutputConfig;
        this.qualifier = qualifier;
    }

    @Override
    public AnalysisTaskResult run() {
        try {
            Files.createDirectories(graphMLOutputConfig.outputDir());
            String graphMLOutputPath = graphMLOutputConfig.outputDir().resolve(graphMLOutputConfig.outputPath()).toAbsolutePath().normalize().toString();
//            new FlowNodeASTTraversal<FlowNode>().accept(astRoot, new FlowNodeSymbolExtractorVisitor(astRoot, dataStructures, null));
            Graph<TypedGraphVertex, TypedGraphEdge> model = exportUnifiedToGraphML(astRoot, dataStructures, qualifier, graphMLOutputPath);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.EXPORT_TO_GRAPHML, model);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.EXPORT_TO_GRAPHML);
        }
    }

    private static Graph<TypedGraphVertex, TypedGraphEdge> exportUnifiedToGraphML(FlowNode astRoot, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, String outputPath) {
        JGraphTGraphBuilder graphMLExporter = new JGraphTGraphBuilder(dataStructures, astRoot, qualifier);
        graphMLExporter.buildAST();
        graphMLExporter.buildCFG();
        graphMLExporter.buildDataStructures();
        graphMLExporter.writeToGraphML(new File(outputPath));
        return graphMLExporter.getModel();
    }
}
