package org.smojol.flowchart;

import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.ParsePipeline;
import org.smojol.analysis.visualisation.ComponentsBuilder;
import org.smojol.common.flowchart.CobolTreeVisualiser;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.ast.FlowchartBuilderImpl;
import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.interpreter.FlowchartGenerationStrategy;
import org.smojol.common.vm.strategy.UnresolvedReferenceThrowStrategy;
import org.smojol.interpreter.SourceConfig;
import org.smojol.interpreter.structure.DefaultFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

public class FlowchartTasks {
    private static final String AST_DIR = "ast";
    private static final String IMAGES_DIR = "images";
    private static final String DOTFILES_DIR = "dotfiles";
    private final String sourceDir;
    private final File[] copyBookPaths;
    private final String dialectJarPath;
    private final String reportRootDir;

    public FlowchartTasks(String sourceDir, String reportRootDir, File[] copyBookPaths, String dialectJarPath) {
        this.sourceDir = sourceDir;
        this.copyBookPaths = copyBookPaths;
        this.dialectJarPath = dialectJarPath;
        this.reportRootDir = reportRootDir;
        report();
    }

    private void report() {
        System.out.println("Parameters passed in \n--------------------");
        System.out.println("srcDir = " + sourceDir);
        System.out.println("reportRootDir = " + reportRootDir);
        System.out.println("dialectJarPath = " + dialectJarPath);
        System.out.println("copyBookPaths = " + copyBookPaths);
    }

    public void generateForPrograms(List<String> programNames, FlowchartGenerationStrategy flowchartGenerationStrategy, LanguageDialect dialect) throws IOException, InterruptedException {
        for (String programName : programNames) {
            generateForProgram(programName, sourceDir, reportRootDir, dialect, flowchartGenerationStrategy);
        }
    }

    private void generateForProgram(String programName, String sourceDir, String reportRootDir, LanguageDialect dialect, FlowchartGenerationStrategy flowchartGenerationStrategy) throws IOException, InterruptedException {
        File source = Paths.get(sourceDir, programName).toFile().getAbsoluteFile();
        String programReportDir = String.format("%s.report", programName);
        Path astOutputDir = Paths.get(reportRootDir, programReportDir, AST_DIR).toAbsolutePath().normalize();
        Path imageOutputDir = Paths.get(reportRootDir, programReportDir, IMAGES_DIR).toAbsolutePath().normalize();
        Path dotFileOutputDir = Paths.get(reportRootDir, programReportDir, DOTFILES_DIR).toAbsolutePath().normalize();
        String cobolParseTreeOutputPath = astOutputDir.resolve(String.format("cobol-%s.json", programName)).toAbsolutePath().normalize().toString();
        String absoluteDialectJarPath = Paths.get(dialectJarPath).toAbsolutePath().normalize().toString();
        SourceConfig sourceConfig = new SourceConfig(programName, source, copyBookPaths, cobolParseTreeOutputPath, absoluteDialectJarPath);

        Files.createDirectories(astOutputDir);
        Files.createDirectories(dotFileOutputDir);
        Files.createDirectories(imageOutputDir);

        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceThrowStrategy(),
                new DefaultFormat1DataStructureBuilder());
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, dialect);

        CobolEntityNavigator navigator = pipeline.parse();
        ParseTree root = navigator.procedureBodyRoot();
        flowchartGenerationStrategy.draw(navigator, root, pipeline, dotFileOutputDir, imageOutputDir, programName);
    }

    public static String outputPath(ParseTree section, Path outputDir, String extension) {
        CobolParser.ProcedureSectionContext s = (CobolParser.ProcedureSectionContext) section;
        String sectionName = s.procedureSectionHeader().sectionName().getText();
        return outputPath(sectionName, outputDir, extension);
    }

    public static String outputPath(String label, Path outputDir, String extension) {
        return outputDir.resolve(String.format("%s.%s", label, extension)).toString();
    }
}
