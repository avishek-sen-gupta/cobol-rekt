package org.smojol.analysis.pipeline;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import com.mojo.woof.Advisor;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.OpenAICredentials;
import org.antlr.v4.runtime.tree.ParseTree;
import org.neo4j.driver.Record;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.graph.DataStructureSummariseAction;
import org.smojol.analysis.graph.NamespaceQualifier;
import org.smojol.analysis.graph.NodeSpecBuilder;
import org.smojol.analysis.graph.SummariseAction;
import org.smojol.analysis.graph.graphml.JGraphTGraphBuilder;
import org.smojol.analysis.graph.neo4j.*;
import org.smojol.analysis.pipeline.config.*;
import org.smojol.ast.ProgramDependencies;
import org.smojol.common.ast.*;
import org.smojol.common.flowchart.*;
import org.smojol.common.id.IdProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.DataStructureNavigator;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.interpreter.*;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static com.mojo.woof.NodeProperties.TYPE;
import static com.mojo.woof.NodeRelations.CONTAINS;

public class SmojolTasks {
    private final SourceConfig sourceConfig;
    private final FlowchartOutputWriter flowchartOutputWriter;
    private final RawASTOutputConfig rawAstOutputConfig;
    private final GraphSDK graphSDK;
    private final GraphMLExportConfig graphMLOutputConfig;
    private final FlowASTOutputConfig flowASTOutputConfig;
    private final CFGOutputConfig cfgOutputConfig;
    private final OutputArtifactConfig dataStructuresOutputConfig;
    private final IdProvider idProvider;
    private final GraphBuildConfig graphBuildConfig;
    private final ParsePipeline pipeline;
    private CobolEntityNavigator navigator;
    private CobolDataStructure dataStructures;
    private final NodeSpecBuilder qualifier;
    private FlowNode astRoot;


    public AnalysisTask INJECT_INTO_NEO4J = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            exportToNeo4J(astRoot, dataStructures, qualifier, graphSDK);
            return AnalysisTaskResult.OK();
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
                return AnalysisTaskResult.OK();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    };

    public AnalysisTask WRITE_DATA_STRUCTURES = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            new WriteDataStructuresTask(dataStructures, dataStructuresOutputConfig).run();
            return AnalysisTaskResult.OK();
        }
    };
    public AnalysisTask EXPORT_TO_GRAPHML = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            try {
                Files.createDirectories(graphMLOutputConfig.outputDir());
                String graphMLOutputPath = graphMLOutputConfig.outputDir().resolve(graphMLOutputConfig.outputPath()).toAbsolutePath().normalize().toString();
                exportToGraphML(astRoot, dataStructures, qualifier, graphMLOutputPath);
                return AnalysisTaskResult.OK();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    };

    public AnalysisTask COMPARE_PARAGRAPHS = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            new CompareCodeBlocksTask(dataStructures, qualifier).run(astRoot, NodeOperationCostFunctions.GENERIC);
            return AnalysisTaskResult.OK();
        }
    };

    public AnalysisTask WRITE_RAW_AST = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            try {
                System.out.println(ConsoleColors.green(String.format("Memory usage: %s", Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())));
                System.out.printf("AST Output Dir is: %s%n", rawAstOutputConfig.astOutputDir());
                Files.createDirectories(rawAstOutputConfig.astOutputDir());
                rawAstOutputConfig.visualiser().writeCobolAST(pipeline.getTree(), rawAstOutputConfig.cobolParseTreeOutputPath(), false, navigator);
                return AnalysisTaskResult.OK();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    };

    public AnalysisTask WRITE_FLOW_AST = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            SerialisableASTFlowNode serialisableASTFlowRoot = new SerialiseFlowASTTask().serialisedFlowAST(astRoot);
            Gson gson = new GsonBuilder().setPrettyPrinting().create();
            try (JsonWriter writer = new JsonWriter(new FileWriter(flowASTOutputConfig.outputPath()))) {
                Files.createDirectories(flowASTOutputConfig.outputDir());
                writer.setIndent("  ");
                gson.toJson(serialisableASTFlowRoot, SerialisableASTFlowNode.class, writer);
                return AnalysisTaskResult.OK();
            } catch (IOException e) {
                throw new RuntimeException(e);
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
                return AnalysisTaskResult.OK();
            } catch (IOException | InterruptedException e) {
                throw new RuntimeException(e);
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
                return AnalysisTaskResult.OK();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    };

    public AnalysisTask BUILD_PROGRAM_DEPENDENCIES = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            return AnalysisTaskResult.OK(new ProgramDependencies(astRoot, sourceConfig.programName()));
        }
    };

    public AnalysisTask SUMMARISE_THROUGH_LLM = new AnalysisTask() {
        @Override
        public AnalysisTaskResult run() {
            summariseThroughLLM(qualifier, graphSDK);
            return AnalysisTaskResult.OK();
        }
    };

    public SmojolTasks(ParsePipeline pipeline, SourceConfig sourceConfig, FlowchartOutputWriter flowchartOutputWriter, RawASTOutputConfig rawAstOutputConfig, GraphMLExportConfig graphMLOutputConfig, FlowASTOutputConfig flowASTOutputConfig, CFGOutputConfig cfgOutputConfig, GraphBuildConfig graphBuildConfig, OutputArtifactConfig dataStructuresOutputConfig, GraphSDK graphSDK, IdProvider idProvider) {
        this.pipeline = pipeline;
        this.sourceConfig = sourceConfig;
        this.flowchartOutputWriter = flowchartOutputWriter;
        this.rawAstOutputConfig = rawAstOutputConfig;
        this.dataStructuresOutputConfig = dataStructuresOutputConfig;
        this.graphSDK = graphSDK;
        this.graphMLOutputConfig = graphMLOutputConfig;
        this.flowASTOutputConfig = flowASTOutputConfig;
        this.cfgOutputConfig = cfgOutputConfig;
        this.idProvider = idProvider;
        this.graphBuildConfig = graphBuildConfig;
        qualifier = new NodeSpecBuilder(new NamespaceQualifier("NEW-CODE"));
    }

    public SmojolTasks build() throws IOException {
        navigator = pipeline.parse();
        dataStructures = pipeline.getDataStructures();

        ParseTree procedure = navigator.procedureBodyRoot();
        FlowchartBuilder flowcharter = pipeline.flowcharter();
        flowcharter.buildFlowAST(procedure).buildControlFlow().buildOverlay();
        astRoot = flowcharter.getRoot();

        return this;
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
            case COMPARE_PARAGRAPHS -> COMPARE_PARAGRAPHS;
        });
    }

    private static void exportToGraphML(FlowNode astRoot, CobolDataStructure dataStructures, NodeSpecBuilder qualifier, String outputPath) {
        JGraphTGraphBuilder graphMLExporter = new JGraphTGraphBuilder(dataStructures, astRoot, qualifier);
        graphMLExporter.buildAST();
        graphMLExporter.buildCFG();
        graphMLExporter.buildDataStructures();
        graphMLExporter.write(new File(outputPath));
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
        sdk.traverse(neo4jProgramRoot, new SummariseAction(advisor, sdk), CONTAINS);
        // Summarises data structures
        sdk.traverse(neo4jDataStructuresRoot, new DataStructureSummariseAction(advisor, sdk), CONTAINS);
    }
}
