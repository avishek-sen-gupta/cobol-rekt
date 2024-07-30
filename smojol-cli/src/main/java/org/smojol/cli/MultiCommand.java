package org.smojol.cli;

import com.google.common.collect.ImmutableList;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.pipeline.CodeTaskRunner;
import org.smojol.analysis.pipeline.AnalysisTask;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.id.UUIDProvider;
import org.smojol.interpreter.FlowchartGenerationStrategy;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

@Command(name = "graph", mixinStandardHelpOptions = true, version = "graph 0.1",
        description = "Implements various operations useful for reverse engineering Cobol code")
public class MultiCommand implements Callable<Integer> {

    @Option(names = {"-dp", "--dialectJarPath"},
            defaultValue = "che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
            description = "Path to dialect .JAR")
    private String dialectJarPath;

    @Option(names = {"-s", "--srcDir"},
            required = true,
            description = "The Cobol source directory")
    private String sourceDir;

    @Option(names = {"-c", "--commands"},
            required = true,
            description = "The commands to run (INJECT_INTO_NEO4J, EXPORT_TO_GRAPHML, WRITE_RAW_AST, DRAW_FLOWCHART, WRITE_FLOW_AST, WRITE_CFG)", split = " ")
    private List<String> commands;

    @CommandLine.Parameters(index = "0..*",
            description = "The programs to analyse", split = " ")
    private List<String> programNames;

    // Can be replaced with a File[] (and the later conversion removed) if we skip default arguments.
    @Option(names = {"-cp", "--copyBooksDir"},
            required = true,
            description = "Copybook directories (repeatable)")
    private List<String> copyBookDirs;

    @Option(names = {"-r", "--reportDir"},
            required = true,
            description = "Output report directory")
    private String reportRootDir;

    @Option(names = {"-d", "--dialect"},
            defaultValue = "COBOL",
            description = "The COBOL dialect (COBOL, IDMS)")
    private String dialect;

    @Option(names = {"-g", "--generation"},
            defaultValue = "PROGRAM",
            description = "The flowchart generation strategy. Valid values are SECTION, PROGRAM, and NODRAW")
    private String flowchartGenerationStrategy;

    @Option(names = {"-v", "--validate"},
            defaultValue = "false",
            description = "Only run syntax validation on the input")
    private boolean isValidate;

    @Override
    public Integer call() throws IOException {
        List<File> copyBookPaths = copyBookDirs.stream().map(c -> Paths.get(c).toAbsolutePath().toFile()).toList();
        CodeTaskRunner taskRunner = new CodeTaskRunner(sourceDir, reportRootDir, copyBookPaths, dialectJarPath, LanguageDialect.dialect(dialect), FlowchartGenerationStrategy.strategy(flowchartGenerationStrategy), new UUIDProvider());
        taskRunner.generateForPrograms(isValidate ? ImmutableList.of() : toGraphTasks(commands), programNames);
        if (!isValidate) return 0;
        System.out.println("Only validating, all other tasks were ignored");
        output(taskRunner.getErrorMap());
        return 0;
    }

    private void output(Map<String, List<SyntaxError>> errorMap) {
        if (errorMap.isEmpty()) System.out.println(ConsoleColors.green("No errors found for programs " + String.join(",", programNames)));
        errorMap.forEach((programName, errors) -> {
            System.out.printf("Program %s%n----------------------", programName);
            errors.forEach(e -> System.out.printf("%s%n", e));
        });
    }

    private List<AnalysisTask> toGraphTasks(List<String> commands) {
        return commands.stream().map(AnalysisTask::valueOf).toList();
    }
}
