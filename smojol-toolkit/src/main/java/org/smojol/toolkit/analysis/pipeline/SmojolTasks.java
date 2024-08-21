package org.smojol.toolkit.analysis.pipeline;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import com.mojo.woof.Advisor;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import com.mojo.woof.OpenAICredentials;
import org.antlr.v4.runtime.tree.ParseTree;
import org.apache.commons.lang3.tuple.Triple;
import org.neo4j.driver.Record;
import org.smojol.toolkit.analysis.graph.DataStructureSummariseAction;
import org.smojol.toolkit.analysis.graph.NamespaceQualifier;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.toolkit.analysis.graph.SummariseAction;
import org.smojol.toolkit.analysis.graph.graphml.JGraphTGraphBuilder;
import org.smojol.toolkit.analysis.graph.graphml.SerialisableUnifiedModel;
import org.smojol.toolkit.analysis.graph.neo4j.Neo4JGraphBuilder;
import org.smojol.toolkit.analysis.graph.neo4j.Neo4JRedefinitionVisitor;
import org.smojol.toolkit.analysis.pipeline.config.*;
import org.smojol.common.ast.ProgramDependencies;
import org.smojol.common.ast.*;
import org.smojol.common.flowchart.*;
import org.smojol.common.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.DataStructureNavigator;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.flowchart.FlowchartOutputWriter;
import org.smojol.toolkit.analysis.graph.neo4j.Neo4JDataStructureVisitor;
import org.smojol.toolkit.analysis.graph.neo4j.Neo4JFlowCFGVisitor;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static com.mojo.woof.NodeProperties.TYPE;
import static com.mojo.woof.NodeRelations.*;

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
    private final IdProvider idProvider;
    private final GraphBuildConfig graphBuildConfig;
    private final Neo4JDriverBuilder neo4JDriverBuilder;
    private final ParsePipeline pipeline;
    private CobolEntityNavigator navigator;
    private CobolDataStructure dataStructures;
    private final NodeSpecBuilder qualifier;
    private FlowNode astRoot;


    public AnalysisTask INJECT_INTO_NEO4J = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            try (GraphSDK graphSDK = new GraphSDK(neo4JDriverBuilder.fromEnv())) {
                exportToNeo4J(astRoot, dataStructures, qualifier, graphSDK);
                return AnalysisTaskResult.OK(CommandLineAnalysisTask.INJECT_INTO_NEO4J);
            } catch (Exception e) {
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.INJECT_INTO_NEO4J);
            }
        }
    };

    public AnalysisTask ATTACH_COMMENTS = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            try {
                List<CommentBlock> commentBlocks = new CommentExtraction().run(sourceConfig.sourcePath(), pipeline.getNavigator());
                commentBlocks.forEach(cb -> {
                    System.out.println("Attaching comments");
                    FlowNode node = new FlowNodeNavigator(astRoot).findNarrowestByCondition(n -> n.originalText().contains(cb.getCodeContextLine()));
                    if (node != null) node.addComment(cb);
                    else {
                        CobolDataStructure dataStructure = new DataStructureNavigator(dataStructures).findByCondition(ds -> ds.getRawText().contains(cb.getCodeContextLine()));
                        if (dataStructure != null) dataStructure.addComment(cb);
                        else {
                            FlowNode possibleIdmsNode = new FlowNodeNavigator(astRoot).findByCondition(n -> n.originalText().contains(cb.getCodeContextLine()));
                            if (possibleIdmsNode != null) possibleIdmsNode.addComment(cb);
                            else astRoot.addComment(cb);
                        }
                    }
                });
                return AnalysisTaskResult.OK(CommandLineAnalysisTask.ATTACH_COMMENTS);
            } catch (IOException e) {
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.ATTACH_COMMENTS);
            }
        }
    };

    public AnalysisTask WRITE_DATA_STRUCTURES = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            new WriteDataStructuresTask(dataStructures, dataStructuresOutputConfig).run();
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_DATA_STRUCTURES);
        }
    };
    public AnalysisTask EXPORT_TO_GRAPHML = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            try {
                Files.createDirectories(graphMLOutputConfig.outputDir());
                String graphMLOutputPath = graphMLOutputConfig.outputDir().resolve(graphMLOutputConfig.outputPath()).toAbsolutePath().normalize().toString();
                exportUnifiedToGraphML(astRoot, dataStructures, qualifier, graphMLOutputPath);
                return AnalysisTaskResult.OK(CommandLineAnalysisTask.EXPORT_TO_GRAPHML);
            } catch (IOException e) {
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.EXPORT_TO_GRAPHML);
            }
        }
    };

    public AnalysisTask COMPARE_CODE = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            AnalysisTaskResult result = new CompareCodeBlocksTask(dataStructures, qualifier).run(astRoot, NodeOperationCostFunctions.GENERIC);
            return switch (result) {
                case AnalysisTaskResultOK o -> exportToJSON((List<SimilarityResult>) o.getDetail());
                case AnalysisTaskResultError e -> e;
            };
        }
    };

    public AnalysisTask EXPORT_UNIFIED_TO_JSON = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return exportUnifiedToJSON(astRoot, dataStructures, qualifier, unifiedModelOutputConfig);
        }
    };

    private AnalysisTaskResult exportToJSON(List<SimilarityResult> similarityResults) {
        List<SerialisableSimilarityResult> serialisableResults = similarityResults.stream().map(s ->
                new SerialisableSimilarityResult(s.nodes(), s.distance(), s.editOperationLists())).toList();
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        try {
            Files.createDirectories(similarityOutputConfig.outputDir());
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.COMPARE_CODE);
        }
        try (JsonWriter writer = new JsonWriter(new FileWriter(similarityOutputConfig.fullPath()))) {
            writer.setIndent("  ");
            gson.toJson(serialisableResults, List.class, writer);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.COMPARE_CODE);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.COMPARE_CODE);
        }
    }

    public AnalysisTask WRITE_RAW_AST = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            try {
                System.out.println(ConsoleColors.green(String.format("Memory usage: %s", Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())));
                System.out.printf("AST Output Dir is: %s%n", rawAstOutputConfig.astOutputDir());
                Files.createDirectories(rawAstOutputConfig.astOutputDir());
                rawAstOutputConfig.visualiser().writeCobolAST(pipeline.getTree(), rawAstOutputConfig.cobolParseTreeOutputPath(), false, navigator);
                return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_RAW_AST);
            } catch (IOException e) {
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_RAW_AST);
            }
        }
    };

    public AnalysisTask WRITE_FLOW_AST = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            SerialisableASTFlowNode serialisableASTFlowRoot = new SerialiseFlowASTTask().serialisedFlowAST(astRoot);
            Gson gson = new GsonBuilder().setPrettyPrinting().create();
            try {
                Files.createDirectories(flowASTOutputConfig.outputDir());
            } catch (IOException e) {
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_FLOW_AST);
            }
            try (JsonWriter writer = new JsonWriter(new FileWriter(flowASTOutputConfig.outputPath()))) {
                writer.setIndent("  ");
                gson.toJson(serialisableASTFlowRoot, SerialisableASTFlowNode.class, writer);
                return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_FLOW_AST);
            } catch (IOException e) {
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_FLOW_AST);
            }
        }
    };

    public AnalysisTask DRAW_FLOWCHART = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            ParseTree root = navigator.procedureBodyRoot();
            try {
                flowchartOutputWriter.createOutputDirs();
                flowchartOutputWriter.draw(navigator, root, pipeline, sourceConfig);
                return AnalysisTaskResult.OK(CommandLineAnalysisTask.DRAW_FLOWCHART);
            } catch (IOException | InterruptedException e) {
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.DRAW_FLOWCHART);
            }
        }
    };

    public AnalysisTask WRITE_CFG = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            SerialisableCFGGraphCollector cfgGraphCollector = new SerialisableCFGGraphCollector(idProvider);
            astRoot.accept(cfgGraphCollector, -1);
            Gson gson = new GsonBuilder().setPrettyPrinting().create();
            try (JsonWriter writer = new JsonWriter(new FileWriter(cfgOutputConfig.outputPath()))) {
                Files.createDirectories(cfgOutputConfig.outputDir());
                writer.setIndent("  ");  // Optional: for pretty printing
                gson.toJson(cfgGraphCollector, SerialisableCFGGraphCollector.class, writer);
                return AnalysisTaskResult.OK(CommandLineAnalysisTask.WRITE_CFG);
            } catch (IOException e) {
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.WRITE_CFG);
            }
        }
    };

    public AnalysisTask BUILD_PROGRAM_DEPENDENCIES = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES,
                    new ProgramDependencies(astRoot, sourceConfig.programName()));
        }
    };

    public AnalysisTask SUMMARISE_THROUGH_LLM = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            try (GraphSDK graphSDK = new GraphSDK(neo4JDriverBuilder.fromEnv())) {
                summariseThroughLLM(qualifier, graphSDK);
                return AnalysisTaskResult.OK(CommandLineAnalysisTask.SUMMARISE_THROUGH_LLM);
            } catch (Exception e) {
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.SUMMARISE_THROUGH_LLM);
            }
        }
    };

    public SmojolTasks(ParsePipeline pipeline, SourceConfig sourceConfig, FlowchartOutputWriter flowchartOutputWriter, RawASTOutputConfig rawAstOutputConfig, GraphMLExportConfig graphMLOutputConfig, FlowASTOutputConfig flowASTOutputConfig, CFGOutputConfig cfgOutputConfig, GraphBuildConfig graphBuildConfig, OutputArtifactConfig dataStructuresOutputConfig, OutputArtifactConfig unifiedModelOutputConfig, OutputArtifactConfig similarityOutputConfig, IdProvider idProvider, Neo4JDriverBuilder neo4JDriverBuilder) {
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
        this.idProvider = idProvider;
        this.graphBuildConfig = graphBuildConfig;
        this.neo4JDriverBuilder = neo4JDriverBuilder;
        qualifier = new NodeSpecBuilder(new NamespaceQualifier("NEW-CODE"));
    }

    public SmojolTasks build() throws IOException {
        Triple<CobolEntityNavigator, CobolDataStructure, FlowNode> parseEntities = buildRawAST(pipeline);
        navigator = parseEntities.getLeft();
        dataStructures = parseEntities.getMiddle();
        astRoot = parseEntities.getRight();
        return this;
    }

    private static Triple<CobolEntityNavigator, CobolDataStructure, FlowNode> buildRawAST(ParsePipeline pipeline) throws IOException {
        CobolEntityNavigator navigator = pipeline.parse();
        CobolDataStructure dataStructures = pipeline.getDataStructures();

        ParseTree procedure = navigator.procedureBodyRoot();
        FlowchartBuilder flowcharter = pipeline.flowcharter();
        flowcharter.buildFlowAST(procedure).buildControlFlow().buildOverlay();
        FlowNode astRoot = flowcharter.getRoot();

        return Triple.of(navigator, dataStructures, astRoot);
    }

    public List<AnalysisTaskResult> run(List<CommandLineAnalysisTask> commandLineAnalysisTasks) throws IOException {
        return tasks(commandLineAnalysisTasks).map(AnalysisTask::run).toList();
    }

    private Stream<AnalysisTask> tasks(List<CommandLineAnalysisTask> commandLineAnalysisTasks) {
        return commandLineAnalysisTasks.stream().map(t -> switch (t) {
            case INJECT_INTO_NEO4J -> INJECT_INTO_NEO4J;
            case EXPORT_TO_GRAPHML -> EXPORT_TO_GRAPHML;
            case WRITE_RAW_AST -> WRITE_RAW_AST;
            case DRAW_FLOWCHART -> DRAW_FLOWCHART;
            case WRITE_FLOW_AST -> WRITE_FLOW_AST;
            case WRITE_CFG -> WRITE_CFG;
            case ATTACH_COMMENTS -> ATTACH_COMMENTS;
            case WRITE_DATA_STRUCTURES -> WRITE_DATA_STRUCTURES;
            case BUILD_PROGRAM_DEPENDENCIES -> BUILD_PROGRAM_DEPENDENCIES;
            case EXPORT_UNIFIED_TO_JSON -> EXPORT_UNIFIED_TO_JSON;
            case COMPARE_CODE -> COMPARE_CODE;
            case SUMMARISE_THROUGH_LLM -> SUMMARISE_THROUGH_LLM;
        });
    }

    private static void exportUnifiedToGraphML(FlowNode astRoot, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, String outputPath) {
        JGraphTGraphBuilder graphMLExporter = new JGraphTGraphBuilder(dataStructures, astRoot, qualifier);
        graphMLExporter.buildAST();
        graphMLExporter.buildCFG();
        graphMLExporter.buildDataStructures();
        graphMLExporter.writeToGraphML(new File(outputPath));
    }

    private static AnalysisTaskResult exportUnifiedToJSON(FlowNode astRoot, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, OutputArtifactConfig outputArtifactConfig) {
        JGraphTGraphBuilder graphMLExporter = new JGraphTGraphBuilder(dataStructures, astRoot, qualifier);
        graphMLExporter.buildAST();
        graphMLExporter.buildCFG();
        graphMLExporter.buildDataStructures();
        SerialisableUnifiedModel unifiedModel = graphMLExporter.asSerialisable();
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        Path parentDir = outputArtifactConfig.outputDir();
        if (parentDir != null) {
            try {
                Files.createDirectories(parentDir);
            } catch (IOException e) {
                return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.EXPORT_UNIFIED_TO_JSON);
            }
        }
        try (JsonWriter writer = new JsonWriter(new FileWriter(outputArtifactConfig.fullPath()))) {
            writer.setIndent("  ");
            gson.toJson(unifiedModel, SerialisableUnifiedModel.class, writer);
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.EXPORT_UNIFIED_TO_JSON);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.EXPORT_UNIFIED_TO_JSON);
        }
    }

    private void exportToNeo4J(FlowNode root, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, GraphSDK sdk) {
        // Builds Control Flow Graph
        root.accept(new Neo4JFlowCFGVisitor(sdk, qualifier), -1);

        // Builds AST
        Neo4JGraphBuilder neo4JExporter = new Neo4JGraphBuilder(sdk, dataStructures, qualifier, graphBuildConfig);
        neo4JExporter.buildAST(root);

        // Builds data structures
        dataStructures.accept(new Neo4JDataStructureVisitor(sdk, qualifier), null, n -> false, dataStructures);
        dataStructures.accept(new Neo4JRedefinitionVisitor(sdk, qualifier), null, n -> false, dataStructures);

        // Builds data dependencies
        neo4JExporter.buildDataDependencies(root);
    }

    private static void summariseThroughLLM(NodeSpecBuilder qualifier, GraphSDK sdk) {
        Record neo4jProgramRoot = sdk.findNodes(qualifier.astNodeCriteria(Map.of(TYPE, FlowNodeType.PROCEDURE_DIVISION_BODY.toString()))).getFirst();
        Record neo4jDataStructuresRoot = sdk.findNodes(qualifier.dataNodeSearchCriteria(Map.of(TYPE, "ROOT"))).getFirst();
        Advisor advisor = new Advisor(OpenAICredentials.fromEnv());
        // Summarises AST bottom-up
        sdk.traverse(neo4jProgramRoot, new SummariseAction(advisor, sdk), CONTAINS_CODE);
        // Summarises data structures
        sdk.traverse(neo4jDataStructuresRoot, new DataStructureSummariseAction(advisor, sdk), CONTAINS_DATA);
    }
}
