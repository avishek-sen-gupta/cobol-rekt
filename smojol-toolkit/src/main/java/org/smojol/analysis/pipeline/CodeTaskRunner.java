package org.smojol.analysis.pipeline;

import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import lombok.Getter;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.smojol.analysis.ParseDiagnosticRuntimeError;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.graph.neo4j.NodeReferenceStrategy;
import org.smojol.analysis.pipeline.config.*;
import org.smojol.analysis.visualisation.ComponentsBuilder;
import org.smojol.ast.FlowchartBuilderImpl;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.id.IdProvider;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;
import org.smojol.common.vm.structure.Format1DataStructureBuilder;
import org.smojol.interpreter.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

public class CodeTaskRunner {
    private static final String AST_DIR = "ast";
    private static final String DATA_STRUCTURES_DIR = "data_structures";
    private static final String FLOW_AST_DIR = "flow_ast";
    private static final String IMAGES_DIR = "flowcharts";
    private static final String DOTFILES_DIR = "dotfiles";
    private static final String GRAPHML_DIR = "graphml";
    private static final String CFG_DIR = "cfg";
    private static final String SIMILARITY_DIR = "similarity";
    private static final String UNIFIED_MODEL_DIR = "unified_model";
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

    public CodeTaskRunner(String sourceDir, String reportRootDir, List<File> copyBookPaths, String dialectJarPath, LanguageDialect dialect, FlowchartGenerationStrategy flowchartGenerationStrategy, IdProvider idProvider, Format1DataStructureBuilder format1DataStructureBuilder) {
        this.sourceDir = sourceDir;
        this.copyBookPaths = copyBookPaths;
        this.dialectJarPath = dialectJarPath;
        this.reportRootDir = reportRootDir;
        this.dialect = dialect;
        this.flowchartGenerationStrategy = flowchartGenerationStrategy;
        this.idProvider = idProvider;
        this.format1DataStructureBuilder = format1DataStructureBuilder;
        reportParameters();
    }

    private void reportParameters() {
        System.out.println("Parameters passed in \n--------------------");
        System.out.println("srcDir = " + sourceDir);
        System.out.println("reportRootDir = " + reportRootDir);
        System.out.println("dialectJarPath = " + dialectJarPath);
        System.out.println("copyBookPaths = " + String.join(",", copyBookPaths.stream().map(cp -> cp.toString() + "\n").toList()));
    }

    public Map<String, List<AnalysisTaskResult>> generateForPrograms(List<CommandLineAnalysisTask> tasks, List<String> programFilenames, TaskRunnerMode runnerMode) throws IOException {
        Map<String, List<AnalysisTaskResult>> results = new HashMap<>();
        for (String programFilename : programFilenames) {
            System.out.println(String.format("Running tasks: %s for program '%s' in %s mode...",
                    tasks.stream().map(CommandLineAnalysisTask::name).toList(),
                    programFilename, runnerMode.toString()));
            try {
                Pair<File, String> programPath = new ProgramSearch().run(programFilename, sourceDir);
                if (programPath == ProgramSearch.NO_PATH) {
                    System.out.printf("No program found for '%s' anywhere in path %s \n", programFilename, sourceDir);
                    continue;
                }
                List<AnalysisTaskResult> analysisTaskResults = generateForProgram(programFilename, programPath.getRight(), reportRootDir, this.dialect, runnerMode.tasks(tasks));
                results.put(programFilename, analysisTaskResults);
            } catch (ParseDiagnosticRuntimeError e) {
                errorMap.put(programFilename, e.getErrors());
            }
        }

        return runnerMode.run(errorMap, results, this);
    }

    public Map<String, List<AnalysisTaskResult>> generateForPrograms(List<CommandLineAnalysisTask> tasks, List<String> programFilenames) throws IOException {
        return generateForPrograms(tasks, programFilenames, TaskRunnerMode.PRODUCTION_MODE);
    }

    private List<AnalysisTaskResult> generateForProgram(String programFilename, String sourceDir, String reportRootDir, LanguageDialect dialect, List<CommandLineAnalysisTask> tasks) throws IOException {
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
        String graphMLExportOutputPath = graphMLExportOutputDir.resolve(String.format("%s.graphml", programFilename)).toAbsolutePath().normalize().toString();
        String cfgOutputPath = cfgOutputDir.resolve(String.format("cfg-%s.json", programFilename)).toAbsolutePath().normalize().toString();
        String cobolParseTreeOutputPath = astOutputDir.resolve(String.format("cobol-%s.json", programFilename)).toAbsolutePath().normalize().toString();
        String flowASTOutputPath = flowASTOutputDir.resolve(String.format("flow-ast-%s.json", programFilename)).toAbsolutePath().normalize().toString();
        String absoluteDialectJarPath = Paths.get(dialectJarPath).toAbsolutePath().normalize().toString();
        SourceConfig sourceConfig = new SourceConfig(programFilename, sourceDir, copyBookPaths, absoluteDialectJarPath);
        OutputArtifactConfig dataStructuresOutputConfig = new OutputArtifactConfig(dataStructuresOutputDir, programFilename + "-data.json");
        OutputArtifactConfig similarityOutputConfig = new OutputArtifactConfig(similarityOutputDir, programFilename + "-similarity.json");
        OutputArtifactConfig unifiedModelOutputConfig = new OutputArtifactConfig(unifiedModelOutputDir, programFilename + "-unified.json");

        FlowchartOutputWriter flowchartOutputWriter = new FlowchartOutputWriter(flowchartGenerationStrategy, dotFileOutputDir, imageOutputDir);
        RawASTOutputConfig rawAstOutputConfig = new RawASTOutputConfig(astOutputDir, cobolParseTreeOutputPath, new CobolTreeVisualiser());
        FlowASTOutputConfig flowASTOutputConfig = new FlowASTOutputConfig(flowASTOutputDir, flowASTOutputPath);
        GraphMLExportConfig graphMLOutputConfig = new GraphMLExportConfig(graphMLExportOutputDir, graphMLExportOutputPath);
        CFGOutputConfig cfgOutputConfig = new CFGOutputConfig(cfgOutputDir, cfgOutputPath);
        GraphSDK sdk = new GraphSDK(new Neo4JDriverBuilder().fromEnv());
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceThrowStrategy(),
                format1DataStructureBuilder, idProvider);
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, rawAstOutputConfig, ops, dialect);
        GraphBuildConfig graphBuildConfig = new GraphBuildConfig(
                NodeReferenceStrategy.EXISTING_CFG_NODE,
                NodeReferenceStrategy.EXISTING_CFG_NODE);

        SmojolTasks pipelineTasks = new SmojolTasks(pipeline,
                sourceConfig, flowchartOutputWriter,
                rawAstOutputConfig, graphMLOutputConfig,
                flowASTOutputConfig, cfgOutputConfig,
                graphBuildConfig, dataStructuresOutputConfig, unifiedModelOutputConfig, similarityOutputConfig, sdk,
                idProvider).build();
        return pipelineTasks.run(tasks);
    }
}
