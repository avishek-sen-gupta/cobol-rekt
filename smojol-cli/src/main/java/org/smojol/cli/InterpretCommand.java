package org.smojol.cli;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

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

    @Option(names = {"-p", "--permissiveSearch"},
            description = "Match filename using looser criteria")
    private boolean isPermissiveSearch;

    @Override
    public Integer call() {
//        List<File> copyBookPaths = copyBookDirs.stream().map(c -> Paths.get(c).toAbsolutePath().toFile()).toList();
//        ProgramSearch programSearch = ProgramSearch.searchStrategy(isPermissiveSearch);
//        return new InterpretTask(programSearch).processPrograms(programName, sourceDir,
//                LanguageDialect.valueOf(dialect), copyBookPaths, dialectJarPath, outputPath, isStrict ? ProgramValidationErrors::IS_COMPLETE_SUCCESS : ProgramValidationErrors::IS_PARTIAL_SUCCESS, DataStructureValidation.NO_BUILD) ? 0 : 1;
        return 0;
    }
}
