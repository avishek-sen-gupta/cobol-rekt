package org.smojol.cli;

import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.logging.LoggingConfig;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.defined.InterpretTask;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.toolkit.interpreter.interpreter.CobolConditionResolver;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

import java.io.File;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.logging.Logger;

@Command(name = "interpret", mixinStandardHelpOptions = true, version = "graph 0.1",
        description = "Interprets the COBOL source")
public class InterpretCommand implements Callable<Integer> {
    private static final Logger LOGGER = Logger.getLogger(InterpretCommand.class.getName());
    @Option(names = {"-dp", "--dialectJarPath"},
            defaultValue = "che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
            description = "Path to dialect .JAR")
    private String dialectJarPath;

    @Option(names = {"-s", "--srcDir"},
            required = true,
            description = "The Cobol source directory")
    private String sourceDir;

    @CommandLine.Parameters(index = "0",
            description = "The program to analyse")
    private String programName;

    @Option(names = {"-cp", "--copyBooksDir"},
            required = true,
            description = "Copybook directories (repeatable)", split = ",")
    private List<String> copyBookDirs;

    @Option(names = {"-d", "--dialect"},
            defaultValue = "COBOL",
            description = "The COBOL dialect (COBOL, IDMS)")
    private String dialect;

    @Option(names = {"-t", "--resolveTactic"},
            defaultValue = "NO",
            description = "The condition resolution strategy (YES, NO, CONSOLE, EVAL)")
    private String resolutionTactic;

    @Option(names = {"-p", "--permissiveSearch"},
            description = "Match filename using looser criteria")
    private boolean isPermissiveSearch;

    @Override
    public Integer call() {
        LoggingConfig.setupLogging();
        Pair<File, String> programPath = ProgramSearch.searchStrategy(isPermissiveSearch).run(programName, sourceDir);
        List<File> copyBookPaths = copyBookDirs.stream().map(c -> Paths.get(c).toAbsolutePath().toFile()).toList();
        SourceConfig sourceConfig = new SourceConfig(programName, programPath.getRight(), copyBookPaths, dialectJarPath);
        new InterpretTask(sourceConfig, LanguageDialect.dialect(dialect), CobolConditionResolver.valueOf(resolutionTactic),
                new LocalFilesystemOperations()).run();
        return 0;
    }
}
