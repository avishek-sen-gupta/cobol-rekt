package org.smojol.toolkit.analysis.defined;

import com.mojo.woof.Neo4JDriverBuilder;
import lombok.Getter;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.toolkit.analysis.pipeline.*;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.analysis.error.ParseDiagnosticRuntimeError;
import org.smojol.toolkit.analysis.graph.neo4j.NodeReferenceStrategy;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.toolkit.analysis.pipeline.config.*;
import org.smojol.toolkit.ast.FlowchartBuilderImpl;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.id.IdProvider;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;
import org.smojol.common.vm.structure.Format1DataStructureBuilder;
import org.smojol.toolkit.flowchart.FlowchartGenerationStrategy;
import org.smojol.toolkit.flowchart.FlowchartOutputWriter;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.SmojolTasks;
import org.smojol.toolkit.task.TaskRunnerMode;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.logging.Logger;

public class CodeTaskRunner {
    private static final Logger LOGGER = Logger.getLogger(CodeTaskRunner.class.getName());
    private static final String AST_DIR = "ast";
    private static final String DATA_STRUCTURES_DIR = "data_structures";
    private static final String FLOW_AST_DIR = "flow_ast";
    private static final String IMAGES_DIR = "flowcharts";
    private static final String DOTFILES_DIR = "dotfiles";
    private static final String GRAPHML_DIR = "graphml";
    private static final String CFG_DIR = "cfg";
    private static final String SIMILARITY_DIR = "similarity";
    private static final String UNIFIED_MODEL_DIR = "unified_model";
    private static final String TRANSPILER_MODEL_DIR = "transpiler_model";
    private static final String MERMAID_DIR = "mermaid";
    private final String sourceDir;
    private final List<File> copyBookPaths;
    private final String dialectJarPath;
    private final String reportRootDir;
    private final LanguageDialect dialect;
    private final FlowchartGenerationStrategy flowchartGenerationStrategy;
    private final IdProvider idProvider;
    @Getter
    private final Map<String, List<SyntaxError>> errorMap = new HashMap<>();
    private final Format1DataStructureBuilder format1DataStructureBuilder;
    private final ProgramSearch programSearch;
    private final ResourceOperations resourceOperations;

    public CodeTaskRunner(String sourceDir, String reportRootDir, List<File> copyBookPaths, String dialectJarPath, LanguageDialect dialect, FlowchartGenerationStrategy flowchartGenerationStrategy, IdProvider idProvider, Format1DataStructureBuilder format1DataStructureBuilder, ProgramSearch programSearch, ResourceOperations resourceOperations) {
        this.sourceDir = sourceDir;
        this.copyBookPaths = copyBookPaths;
        this.dialectJarPath = dialectJarPath;
        this.reportRootDir = reportRootDir;
        this.dialect = dialect;
        this.flowchartGenerationStrategy = flowchartGenerationStrategy;
        this.idProvider = idProvider;
        this.format1DataStructureBuilder = format1DataStructureBuilder;
        this.programSearch = programSearch;
        this.resourceOperations = resourceOperations;
        reportParameters();
    }

    private void reportParameters() {
        LOGGER.info("Parameters passed in \n--------------------");
        LOGGER.info("srcDir = " + sourceDir);
        LOGGER.info("reportRootDir = " + reportRootDir);
        LOGGER.info("dialectJarPath = " + dialectJarPath);
        LOGGER.info("copyBookPaths = " + String.join(",", copyBookPaths.stream().map(cp -> cp.toString() + "\n").toList()));
    }

    public Map<String, List<AnalysisTaskResult>> runForPrograms(List<CommandLineAnalysisTask> tasks, List<String> programFilenames, TaskRunnerMode runnerMode) throws IOException {
        Map<String, List<AnalysisTaskResult>> results = new HashMap<>();
        for (String programFilename : programFilenames) {
            LOGGER.info(String.format("Running tasks: %s for program '%s' in %s mode...",
                    tasks.stream().map(CommandLineAnalysisTask::name).toList(),
                    programFilename, runnerMode.toString()));
            try {
                Pair<File, String> programPath = programSearch.run(programFilename, sourceDir);
                if (programPath == ProgramSearch.NO_PATH) {
                    LOGGER.severe(String.format("No program found for '%s' anywhere in path %s \n", programFilename, sourceDir));
                    continue;
                }
                List<AnalysisTaskResult> analysisTaskResults = runForProgram(programFilename, programPath.getRight(), reportRootDir, this.dialect, runnerMode.tasks(tasks));
                results.put(programFilename, analysisTaskResults);
            } catch (ParseDiagnosticRuntimeError e) {
                errorMap.put(programFilename, e.getErrors());
            }
        }

        return runnerMode.run(errorMap, results);
    }

    public Map<String, List<AnalysisTaskResult>> runForPrograms(List<CommandLineAnalysisTask> tasks, List<String> programFilenames) throws IOException {
        return runForPrograms(tasks, programFilenames, TaskRunnerMode.PRODUCTION_MODE);
    }

    private List<AnalysisTaskResult> runForProgram(String programFilename, String sourceDir, String reportRootDir, LanguageDialect dialect, List<CommandLineAnalysisTask> tasks) throws IOException {
        String programReportDir = String.format("%s.report", programFilename);
        Path astOutputDir = Paths.get(reportRootDir, programReportDir, AST_DIR).toAbsolutePath().normalize();
        Path dataStructuresOutputDir = Paths.get(reportRootDir, programReportDir, DATA_STRUCTURES_DIR).toAbsolutePath().normalize();
        Path flowASTOutputDir = Paths.get(reportRootDir, programReportDir, FLOW_AST_DIR).toAbsolutePath().normalize();
        Path imageOutputDir = Paths.get(reportRootDir, programReportDir, IMAGES_DIR).toAbsolutePath().normalize();
        Path dotFileOutputDir = Paths.get(reportRootDir, programReportDir, DOTFILES_DIR).toAbsolutePath().normalize();
        Path graphMLExportOutputDir = Paths.get(reportRootDir, programReportDir, GRAPHML_DIR).toAbsolutePath().normalize();
        Path cfgOutputDir = Paths.get(reportRootDir, programReportDir, CFG_DIR).toAbsolutePath().normalize();
        Path similarityOutputDir = Paths.get(reportRootDir, programReportDir, SIMILARITY_DIR).toAbsolutePath().normalize();
        Path unifiedModelOutputDir = Paths.get(reportRootDir, programReportDir, UNIFIED_MODEL_DIR).toAbsolutePath().normalize();
        Path transpilerModelOutputDir = Paths.get(reportRootDir, programReportDir, TRANSPILER_MODEL_DIR).toAbsolutePath().normalize();
        String graphMLExportOutputPath = graphMLExportOutputDir.resolve(String.format("%s.graphml", programFilename)).toAbsolutePath().normalize().toString();
        String cfgOutputPath = cfgOutputDir.resolve(String.format("cfg-%s.json", programFilename)).toAbsolutePath().normalize().toString();
        String cobolParseTreeOutputPath = astOutputDir.resolve(String.format("cobol-%s.json", programFilename)).toAbsolutePath().normalize().toString();
        String flowASTOutputPath = flowASTOutputDir.resolve(String.format("flow-ast-%s.json", programFilename)).toAbsolutePath().normalize().toString();
        String absoluteDialectJarPath = Paths.get(dialectJarPath).toAbsolutePath().normalize().toString();
        SourceConfig sourceConfig = new SourceConfig(programFilename, sourceDir, copyBookPaths, absoluteDialectJarPath);
        OutputArtifactConfig dataStructuresOutputConfig = new OutputArtifactConfig(dataStructuresOutputDir, programFilename + "-data.json");
        OutputArtifactConfig similarityOutputConfig = new OutputArtifactConfig(similarityOutputDir, programFilename + "-similarity.json");
        OutputArtifactConfig unifiedModelOutputConfig = new OutputArtifactConfig(unifiedModelOutputDir, programFilename + "-unified.json");
        OutputArtifactConfig transpilerModelOutputConfig = new OutputArtifactConfig(transpilerModelOutputDir, programFilename + "-transpiler-model.json");

        FlowchartOutputWriter flowchartOutputWriter = new FlowchartOutputWriter(flowchartGenerationStrategy, dotFileOutputDir, imageOutputDir);
        RawASTOutputConfig rawAstOutputConfig = new RawASTOutputConfig(astOutputDir, cobolParseTreeOutputPath, new CobolTreeVisualiser());
        OutputArtifactConfig mermaidOutputConfig = new OutputArtifactConfig(Paths.get(reportRootDir, programReportDir, MERMAID_DIR).toAbsolutePath().normalize(), "");
        FlowASTOutputConfig flowASTOutputConfig = new FlowASTOutputConfig(flowASTOutputDir, flowASTOutputPath);
        GraphMLExportConfig graphMLOutputConfig = new GraphMLExportConfig(graphMLExportOutputDir, graphMLExportOutputPath);
        CFGOutputConfig cfgOutputConfig = new CFGOutputConfig(cfgOutputDir, cfgOutputPath);
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(resourceOperations),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceThrowStrategy(),
                format1DataStructureBuilder, idProvider, resourceOperations);
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, dialect);
        GraphBuildConfig graphBuildConfig = new GraphBuildConfig(
                NodeReferenceStrategy.EXISTING_CFG_NODE,
                NodeReferenceStrategy.EXISTING_CFG_NODE);

        SmojolTasks pipelineTasks = new SmojolTasks(pipeline,
                sourceConfig, flowchartOutputWriter,
                rawAstOutputConfig, graphMLOutputConfig,
                flowASTOutputConfig, cfgOutputConfig,
                graphBuildConfig, dataStructuresOutputConfig, unifiedModelOutputConfig, similarityOutputConfig,
                mermaidOutputConfig, transpilerModelOutputConfig,
                idProvider, resourceOperations, new Neo4JDriverBuilder()).build();
        return pipelineTasks.run(tasks);
    }
}
