package org.smojol.toolkit.task;

import com.mojo.woof.Neo4JDriverBuilder;
import org.antlr.v4.runtime.ParserRuleContext;
import org.smojol.common.pseudocode.BasicBlockFactory;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.pseudocode.SymbolReferenceBuilder;
import org.smojol.toolkit.analysis.defined.*;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.analysis.graph.NamespaceQualifier;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.pipeline.config.*;
import org.smojol.common.ast.*;
import org.smojol.common.flowchart.*;
import org.smojol.common.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.flowchart.FlowchartOutputWriter;
import org.smojol.common.resource.ResourceOperations;

import java.io.IOException;
import java.util.List;
import java.util.stream.Stream;

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
    private final IdProvider idProvider;
    private final GraphBuildConfig graphBuildConfig;
    private final ResourceOperations resourceOperations;
    private final Neo4JDriverBuilder neo4JDriverBuilder;
    private final ParsePipeline pipeline;
    private CobolEntityNavigator navigator;
    private CobolDataStructure dataStructures;
    private final NodeSpecBuilder qualifier;
    private FlowNode flowRoot;
    private ParserRuleContext rawAST;
    private SmojolSymbolTable symbolTable;

    public SmojolTasks(ParsePipeline pipeline, SourceConfig sourceConfig, FlowchartOutputWriter flowchartOutputWriter, RawASTOutputConfig rawAstOutputConfig, GraphMLExportConfig graphMLOutputConfig, FlowASTOutputConfig flowASTOutputConfig, CFGOutputConfig cfgOutputConfig, GraphBuildConfig graphBuildConfig, OutputArtifactConfig dataStructuresOutputConfig, OutputArtifactConfig unifiedModelOutputConfig, OutputArtifactConfig similarityOutputConfig, OutputArtifactConfig mermaidOutputConfig, OutputArtifactConfig transpilerModelOutputConfig, IdProvider idProvider, ResourceOperations resourceOperations, Neo4JDriverBuilder neo4JDriverBuilder) {
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
        this.idProvider = idProvider;
        this.graphBuildConfig = graphBuildConfig;
        this.resourceOperations = resourceOperations;
        this.neo4JDriverBuilder = neo4JDriverBuilder;
        qualifier = new NodeSpecBuilder(new NamespaceQualifier("NEW-CODE"));
    }

    public List<AnalysisTaskResult> run(List<CommandLineAnalysisTask> commandLineAnalysisTasks) throws IOException {
        return tasks(commandLineAnalysisTasks).map(AnalysisTask::run).toList();
    }

    public AnalysisTask INJECT_INTO_NEO4J = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new InjectIntoNeo4JTask(flowRoot, dataStructures, qualifier, neo4JDriverBuilder, graphBuildConfig).run();
        }
    };

    public AnalysisTask ATTACH_COMMENTS = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new AttachCommentsTask(flowRoot, dataStructures, navigator, sourceConfig).run();
        }
    };

    public AnalysisTask WRITE_DATA_STRUCTURES = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new WriteDataStructuresTask(dataStructures, dataStructuresOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask EXPORT_TO_GRAPHML = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new ExportToGraphMLTask(flowRoot, dataStructures, graphMLOutputConfig, qualifier, resourceOperations).run();
        }
    };

    public AnalysisTask COMPARE_CODE = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new CompareCodeTask(flowRoot, dataStructures, qualifier, similarityOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask EXPORT_UNIFIED_TO_JSON = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new ExportUnifiedModelToJSONTask(flowRoot, dataStructures, qualifier, unifiedModelOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask WRITE_RAW_AST = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new WriteRawASTTask(navigator, rawAstOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask WRITE_FLOW_AST = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new WriteFlowASTTask(flowRoot, flowASTOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask BUILD_PSEUDOCODE_GRAPH = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new BuildPseudocodeGraphTask(flowRoot, true).run();
        }
    };

    public AnalysisTask ANALYSE_CONTROL_FLOW = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new AnalyseControlFlowTask(flowRoot, new BasicBlockFactory(idProvider), neo4JDriverBuilder).run();
        }
    };

    public AnalysisTask BUILD_TRANSPILER_MODEL = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new BuildTranspilerModelTask(rawAST, dataStructures, symbolTable, transpilerModelOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask GENERATE_IR = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new GenerateIntermediateRepresentationTask(flowRoot, dataStructures, idProvider, neo4JDriverBuilder).run();
        }
    };

    public AnalysisTask DRAW_FLOWCHART = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new DrawFlowchartTask(pipeline.flowcharter(), navigator, flowchartOutputWriter, sourceConfig, resourceOperations).run();
        }
    };

    public AnalysisTask EXPORT_MERMAID = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new ExportMermaidTask(flowRoot, mermaidOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask WRITE_CFG = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new WriteControlFlowGraphTask(flowRoot, idProvider, cfgOutputConfig, resourceOperations).run();
        }
    };

    public AnalysisTask BUILD_PROGRAM_DEPENDENCIES = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new ProgramDependenciesTask(flowRoot, sourceConfig.programName()).run();
        }
    };

    public AnalysisTask SUMMARISE_THROUGH_LLM = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return new SummariseThroughLLMTask(neo4JDriverBuilder, qualifier).run();
        }
    };

    public SmojolTasks build() throws IOException {
        navigator = pipeline.parse();
        rawAST = (ParserRuleContext) navigator.procedureBodyRoot();
        dataStructures = pipeline.getDataStructures();
        FlowchartBuilder flowcharter = pipeline.flowcharter();
        symbolTable = new SmojolSymbolTable(dataStructures, new SymbolReferenceBuilder(idProvider));
        flowcharter.buildFlowAST(rawAST).buildControlFlow().buildOverlay();
        flowRoot = flowcharter.getRoot();
        flowRoot.resolve(symbolTable, dataStructures);
//        new FlowNodeASTTraversal<FlowNode>().accept(astRoot, new FlowNodeSymbolExtractorVisitor(astRoot, dataStructRoot, symbolTable));
        return this;
    }

    private Stream<AnalysisTask> tasks(List<CommandLineAnalysisTask> commandLineAnalysisTasks) {
        return commandLineAnalysisTasks.stream().map(t -> switch (t) {
            case INJECT_INTO_NEO4J -> INJECT_INTO_NEO4J;
            case EXPORT_TO_GRAPHML -> EXPORT_TO_GRAPHML;
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
            case SUMMARISE_THROUGH_LLM -> SUMMARISE_THROUGH_LLM;
            case BUILD_PSEUDOCODE_GRAPH -> BUILD_PSEUDOCODE_GRAPH;
            case ANALYSE_CONTROL_FLOW -> ANALYSE_CONTROL_FLOW;
            case BUILD_TRANSPILER_MODEL -> BUILD_TRANSPILER_MODEL;
            case GENERATE_IR -> nullTask(CommandLineAnalysisTask.GENERATE_IR);
        });
    }

    private AnalysisTask nullTask(CommandLineAnalysisTask task) {
        return () -> new AnalysisTaskResultOK(task.name(), task + " is not live yet.");
    }
}
