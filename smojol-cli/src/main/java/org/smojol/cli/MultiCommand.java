package org.smojol.cli;

import org.smojol.common.logging.LoggingConfig;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.defined.CodeTaskRunner;
import org.smojol.toolkit.analysis.validation.DataStructureValidation;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.common.validation.ProgramValidationErrors;
import org.smojol.toolkit.analysis.pipeline.*;
import org.smojol.toolkit.analysis.validation.ValidateTaskRunner;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.id.UUIDProvider;
import org.smojol.toolkit.flowchart.FlowchartGenerationStrategy;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.TaskRunnerMode;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.logging.Logger;

@Command(name = "run", mixinStandardHelpOptions = true, version = "graph 0.1",
        description = "Implements various operations useful for reverse engineering Cobol code")
public class MultiCommand implements Callable<Integer> {
    private static final Logger LOGGER = Logger.getLogger(MultiCommand.class.getName());
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
            description = "The commands to run (INJECT_INTO_NEO4J, EXPORT_TO_GRAPHML, WRITE_RAW_AST, DRAW_FLOWCHART, WRITE_FLOW_AST, WRITE_CFG, ATTACH_COMMENTS, WRITE_DATA_STRUCTURES, BUILD_PROGRAM_DEPENDENCIES, COMPARE_CODE, EXPORT_UNIFIED_TO_JSON, EXPORT_MERMAID)", split = " ")
    private List<String> commands;

    @CommandLine.Parameters(index = "0..*",
            description = "The programs to analyse", split = " ")
    private List<String> programNames;

    // Can be replaced with a File[] (and the later conversion removed) if we skip default arguments.
    @Option(names = {"-cp", "--copyBooksDir"},
            required = true,
            description = "Copybook directories (repeatable)", split = ",")
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

    @Option(names = {"-f", "--flowchartOutputFormat"},
            defaultValue = "svg",
            description = "Format of the flowchart output (PNG, SVG)")
    private String flowchartOutputFormat;

    @Option(names = {"-p", "--permissiveSearch"},
            description = "Match filename using looser criteria")
    private boolean isPermissiveSearch;

    @Override
    public Integer call() throws IOException {
        LoggingConfig.setupLogging();
        List<File> copyBookPaths = copyBookDirs.stream().map(c -> Paths.get(c).toAbsolutePath().toFile()).toList();
        return processPrograms(copyBookPaths);
    }

    private Integer processPrograms(List<File> copyBookPaths) throws IOException {
        ProgramSearch programSearch = ProgramSearch.searchStrategy(isPermissiveSearch);
        if (isValidate) {
            LOGGER.info("Only validating, all other tasks were ignored");
            boolean validationResult = new ValidateTaskRunner(programSearch).processPrograms(programNames, sourceDir, LanguageDialect.dialect(dialect), copyBookPaths, dialectJarPath, null, ProgramValidationErrors::IS_PARTIAL_SUCCESS, DataStructureValidation.BUILD);
            return validationResult ? 0 : 1;
        }

        CodeTaskRunner taskRunner = new CodeTaskRunner(sourceDir, reportRootDir, copyBookPaths, dialectJarPath, LanguageDialect.dialect(dialect), FlowchartGenerationStrategy.strategy(flowchartGenerationStrategy, flowchartOutputFormat), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), programSearch, new LocalFilesystemOperations());
        copyBookPaths.forEach(cpp -> LOGGER.info(cpp.getAbsolutePath()));
        Map<String, List<AnalysisTaskResult>> runResults = taskRunner.runForPrograms(toGraphTasks(commands), programNames, TaskRunnerMode.PRODUCTION_MODE);
        return processResults(runResults);
    }

    private Integer processResults(Map<String, List<AnalysisTaskResult>> runResults) {
        List<String> resultMessages = runResults.entrySet().stream().map(this::taskResults).toList();
        Boolean allSucceeded = runResults.values().stream().map(rs ->
                        rs.stream().map(AnalysisTaskResult::isSuccess)
                                .reduce(true, (innerAll, x) -> innerAll && x))
                .reduce((a, b) -> a && b).get();
        resultMessages.forEach(LOGGER::info);
        return allSucceeded ? 0 : 1;
    }

    private String taskResults(Map.Entry<String, List<AnalysisTaskResult>> taskResult) {
        return String.join("\n", taskResult.getValue().stream().map(e -> taskResult.getKey()
                + " -> " + (e.isSuccess() ? ConsoleColors.green(e.toString()) : ConsoleColors.red(e.toString()))).toList()) + "\n";
    }

    private List<CommandLineAnalysisTask> toGraphTasks(List<String> commands) {
        return commands.stream().map(CommandLineAnalysisTask::valueOf).toList();
    }
}
