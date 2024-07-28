package org.smojol.flowchart;

import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.graph.neo4j.NodeReferenceStrategy;
import org.smojol.analysis.pipeline.SmojolPipelineTasks;
import org.smojol.analysis.visualisation.ComponentsBuilder;
import org.smojol.ast.FlowchartBuilderImpl;
import org.smojol.common.flowchart.CobolTreeVisualiser;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;
import org.smojol.interpreter.*;
import org.smojol.interpreter.structure.DefaultFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

public class GraphDBTasks {
    private static final String AST_DIR = "ast";
    private static final String IMAGES_DIR = "images";
    private static final String DOTFILES_DIR = "dotfiles";
    private static final String GRAPHML_DIR = "graphml";
    private final String sourceDir;
    private final File[] copyBookPaths;
    private final String dialectJarPath;
    private final String reportRootDir;
    private final LanguageDialect dialect;
    private final FlowchartGenerationStrategy flowchartGenerationStrategy;

    public GraphDBTasks(String sourceDir, String reportRootDir, File[] copyBookPaths, String dialectJarPath, LanguageDialect dialect, FlowchartGenerationStrategy flowchartGenerationStrategy) {
        this.sourceDir = sourceDir;
        this.copyBookPaths = copyBookPaths;
        this.dialectJarPath = dialectJarPath;
        this.reportRootDir = reportRootDir;
        this.dialect = dialect;
        this.flowchartGenerationStrategy = flowchartGenerationStrategy;
        report();
    }

    private void report() {
        System.out.println("Parameters passed in \n--------------------");
        System.out.println("srcDir = " + sourceDir);
        System.out.println("reportRootDir = " + reportRootDir);
        System.out.println("dialectJarPath = " + dialectJarPath);
        System.out.println("copyBookPaths = " + copyBookPaths);
    }

    public void generateForPrograms(List<String> programFilenames, List<GraphCLITask> tasks) throws IOException {
        for (String programFilename : programFilenames) {
            generateForProgram(programFilename, sourceDir, reportRootDir, this.dialect, tasks);
        }
    }

    private void generateForProgram(String programFilename, String sourceDir, String reportRootDir, LanguageDialect dialect, List<GraphCLITask> tasks) throws IOException {
        File source = Paths.get(sourceDir, programFilename).toFile().getAbsoluteFile();
        String programReportDir = String.format("%s.report", programFilename);
        Path astOutputDir = Paths.get(reportRootDir, programReportDir, AST_DIR).toAbsolutePath().normalize();
        Path imageOutputDir = Paths.get(reportRootDir, programReportDir, IMAGES_DIR).toAbsolutePath().normalize();
        Path dotFileOutputDir = Paths.get(reportRootDir, programReportDir, DOTFILES_DIR).toAbsolutePath().normalize();
        Path graphMLExportOutputDir = Paths.get(reportRootDir, programReportDir, GRAPHML_DIR).toAbsolutePath().normalize();
        String graphMLExportOutputPath = graphMLExportOutputDir.resolve(String.format("%s.graphml", programFilename)).toAbsolutePath().normalize().toString();
        String cobolParseTreeOutputPath = astOutputDir.resolve(String.format("cobol-%s.json", programFilename)).toAbsolutePath().normalize().toString();
        String absoluteDialectJarPath = Paths.get(dialectJarPath).toAbsolutePath().normalize().toString();
        SourceConfig sourceConfig = new SourceConfig(programFilename, source, copyBookPaths, cobolParseTreeOutputPath, absoluteDialectJarPath);

        FlowchartOutputConfig flowchartOutputConfig = new FlowchartOutputConfig(astOutputDir, dotFileOutputDir, imageOutputDir, flowchartGenerationStrategy);
        ASTOutputConfig astOutputConfig = new ASTOutputConfig(astOutputDir, new CobolTreeVisualiser());
        GraphMLExportConfig graphMLOutputConfig = new GraphMLExportConfig(graphMLExportOutputDir, graphMLExportOutputPath);
        GraphSDK sdk = new GraphSDK(new Neo4JDriverBuilder().fromEnv());
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceThrowStrategy(),
                new DefaultFormat1DataStructureBuilder());
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, dialect);

        SmojolPipelineTasks pipelineTasks = new SmojolPipelineTasks(pipeline,
                NodeReferenceStrategy.EXISTING_CFG_NODE,
                NodeReferenceStrategy.EXISTING_CFG_NODE,
                sourceConfig, flowchartOutputConfig, astOutputConfig, sdk, graphMLOutputConfig).build();
        pipelineTasks.run(tasks.stream().map(t -> switch (t) {
            case INJECT_INTO_NEO4J -> pipelineTasks.INJECT_INTO_NEO4J;
            case EXPORT_TO_GRAPHML -> pipelineTasks.EXPORT_TO_GRAPHML;
            case WRITE_RAW_AST -> pipelineTasks.WRITE_RAW_AST;
            case DRAW_FLOWCHART -> pipelineTasks.DRAW_FLOWCHART;
        }).toList());
    }
}
