package org.smojol.toolkit.task;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.task.*;
import com.mojo.algorithms.transpiler.TranspilerFlowgraph;
import com.mojo.woof.Neo4JDriverBuilder;
import com.mojo.algorithms.id.IdProvider;
import org.smojol.common.resource.ResourceOperations;
import com.mojo.algorithms.graph.NamespaceQualifier;
import org.smojol.toolkit.intermediate.NodeSpecBuilder;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisModel;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.analysis.pipeline.config.*;
import org.smojol.toolkit.analysis.task.analysis.*;
import org.smojol.toolkit.flowchart.FlowchartOutputWriter;

import java.io.IOException;
import java.util.List;
import java.util.stream.Stream;

import static com.mojo.algorithms.task.CommandLineAnalysisTask.DO_NOTHING;

public class SmojolTasks {
    private final SourceConfig sourceConfig;
    private final FlowchartOutputWriter flowchartOutputWriter;
    private final RawASTOutputConfig rawAstOutputConfig;
    private final OutputArtifactConfig unifiedModelOutputConfig;
    private final OutputArtifactConfig similarityOutputConfig;
    private final GraphMLExportConfig graphMLOutputConfig;
    private final FlowASTOutputConfig flowASTOutputConfig;
    private final CFGOutputConfig cfgOutputConfig;
    private final OutputArtifactConfig dataStructuresOutputConfig;
    private final OutputArtifactConfig mermaidOutputConfig;
    private final OutputArtifactConfig transpilerModelOutputConfig;
    private final GraphBuildConfig graphBuildConfig;
    private final OutputArtifactConfig llmOutputConfig;
    private final IdProvider idProvider;
    private final ResourceOperations resourceOperations;
    private final Neo4JDriverBuilder neo4JDriverBuilder;
    private final ParsePipeline pipeline;
    private final NodeSpecBuilder qualifier;
    private BaseAnalysisModel baseModel;

    public SmojolTasks(ParsePipeline pipeline, SourceConfig sourceConfig, FlowchartOutputWriter flowchartOutputWriter, RawASTOutputConfig rawAstOutputConfig, GraphMLExportConfig graphMLOutputConfig, FlowASTOutputConfig flowASTOutputConfig, CFGOutputConfig cfgOutputConfig, GraphBuildConfig graphBuildConfig, OutputArtifactConfig dataStructuresOutputConfig, OutputArtifactConfig unifiedModelOutputConfig, OutputArtifactConfig similarityOutputConfig, OutputArtifactConfig mermaidOutputConfig, OutputArtifactConfig transpilerModelOutputConfig, OutputArtifactConfig llmOutputConfig, IdProvider idProvider, ResourceOperations resourceOperations, Neo4JDriverBuilder neo4JDriverBuilder) {
        this.pipeline = pipeline;
        this.sourceConfig = sourceConfig;
        this.flowchartOutputWriter = flowchartOutputWriter;
        this.rawAstOutputConfig = rawAstOutputConfig;
        this.dataStructuresOutputConfig = dataStructuresOutputConfig;
        this.unifiedModelOutputConfig = unifiedModelOutputConfig;
        this.similarityOutputConfig = similarityOutputConfig;
        this.graphMLOutputConfig = graphMLOutputConfig;
        this.flowASTOutputConfig = flowASTOutputConfig;
        this.cfgOutputConfig = cfgOutputConfig;
        this.mermaidOutputConfig = mermaidOutputConfig;
        this.transpilerModelOutputConfig = transpilerModelOutputConfig;
        this.llmOutputConfig = llmOutputConfig;
        this.idProvider = idProvider;
        this.graphBuildConfig = graphBuildConfig;
        this.resourceOperations = resourceOperations;
        this.neo4JDriverBuilder = neo4JDriverBuilder;
        qualifier = new NodeSpecBuilder(new NamespaceQualifier("NEW-CODE"));
    }

    public List<AnalysisTaskResult> run(List<CommandLineAnalysisTask> commandLineAnalysisTasks) throws IOException {
        return tasks(commandLineAnalysisTasks).map(AnalysisTask::run).toList();
    }

    public AnalysisTask FLOW_TO_NEO4J = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new InjectIntoNeo4JTask(baseModel.flowRoot(), baseModel.dataStructures(), qualifier, neo4JDriverBuilder, graphBuildConfig).run();
        }
    };

    public AnalysisTask ATTACH_COMMENTS = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new AttachCommentsTask(baseModel.flowRoot(), baseModel.dataStructures(), baseModel.navigator(), sourceConfig).run();
        }
    };

    public AnalysisTask WRITE_DATA_STRUCTURES = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new WriteDataStructuresTask(baseModel.dataStructures(), dataStructuresOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask FLOW_TO_GRAPHML = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new ExportToGraphMLTask(baseModel.flowRoot(), baseModel.dataStructures(), graphMLOutputConfig, qualifier, resourceOperations).run();
        }
    };

    public AnalysisTask COMPARE_CODE = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new CompareCodeTask(baseModel.flowRoot(), baseModel.dataStructures(), qualifier, similarityOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask EXPORT_UNIFIED_TO_JSON = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new ExportUnifiedModelToJSONTask(baseModel.flowRoot(), baseModel.dataStructures(), qualifier, unifiedModelOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask WRITE_RAW_AST = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new WriteRawASTTask(baseModel.navigator(), rawAstOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask WRITE_FLOW_AST = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new WriteFlowASTTask(baseModel.flowRoot(), flowASTOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask BUILD_TRANSPILER_FLOWGRAPH = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            TranspilerFlowgraph transpilerFlowgraph = new BuildTranspilerFlowgraphTask(baseModel.rawAST(), baseModel.dataStructures(), baseModel.symbolTable(), ImmutableList.of("MAIN-SECTION-01", "SUBROUTINE-1"), transpilerModelOutputConfig, resourceOperations, neo4JDriverBuilder).run();
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH, transpilerFlowgraph);
        }
    };

    public AnalysisTask DRAW_FLOWCHART = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new DrawFlowchartTask(baseModel.navigator(), flowchartOutputWriter, sourceConfig, resourceOperations, baseModel.flowRoot()).run();
        }
    };

    public AnalysisTask EXPORT_MERMAID = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new ExportMermaidTask(baseModel.flowRoot(), mermaidOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask WRITE_CFG = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new WriteControlFlowGraphTask(baseModel.flowRoot(), idProvider, cfgOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask BUILD_PROGRAM_DEPENDENCIES = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new ProgramDependenciesTask(baseModel.flowRoot()).run();
        }
    };

    public AnalysisTask SUMMARISE_THROUGH_LLM = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new SummariseThroughLLMTask(neo4JDriverBuilder, qualifier).run();
        }
    };

    public AnalysisTask WRITE_LLM_SUMMARY = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new WriteLLMSummaryTask(baseModel.flowRoot(), baseModel.dataStructures(), llmOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask BUILD_BASE_ANALYSIS = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            AnalysisTaskResult result = new BuildBaseModelTask(pipeline, idProvider).run();
            return switch (result) {
                case AnalysisTaskResultOK o -> {
                    baseModel = o.getDetail();
                    yield o;
                }

                case AnalysisTaskResultError e -> e;
            };
        }
    };

    private Stream<AnalysisTask> tasks(List<CommandLineAnalysisTask> commandLineAnalysisTasks) {
        return commandLineAnalysisTasks.stream().map(t -> switch (t) {
            case BUILD_BASE_ANALYSIS -> BUILD_BASE_ANALYSIS;
            case FLOW_TO_NEO4J -> FLOW_TO_NEO4J;
            case FLOW_TO_GRAPHML -> FLOW_TO_GRAPHML;
            case WRITE_RAW_AST -> WRITE_RAW_AST;
            case DRAW_FLOWCHART -> DRAW_FLOWCHART;
            case EXPORT_MERMAID -> EXPORT_MERMAID;
            case WRITE_FLOW_AST -> WRITE_FLOW_AST;
            case WRITE_CFG -> WRITE_CFG;
            case ATTACH_COMMENTS -> ATTACH_COMMENTS;
            case WRITE_DATA_STRUCTURES -> WRITE_DATA_STRUCTURES;
            case BUILD_PROGRAM_DEPENDENCIES -> BUILD_PROGRAM_DEPENDENCIES;
            case EXPORT_UNIFIED_TO_JSON -> EXPORT_UNIFIED_TO_JSON;
            case COMPARE_CODE -> COMPARE_CODE;
            case WRITE_LLM_SUMMARY -> WRITE_LLM_SUMMARY;
            case SUMMARISE_THROUGH_LLM -> SUMMARISE_THROUGH_LLM;
            case BUILD_TRANSPILER_FLOWGRAPH -> BUILD_TRANSPILER_FLOWGRAPH;
            case DO_NOTHING -> nullTask(DO_NOTHING);
        });
    }

    private AnalysisTask nullTask(CommandLineAnalysisTask task) {
        return () -> new AnalysisTaskResultOK(task.name(), baseModel);
    }
}
