package org.smojol.cli;

import org.smojol.common.validation.ProgramValidationErrors;
import org.smojol.toolkit.analysis.validation.ValidateTaskRunner;
import org.smojol.common.dialect.LanguageDialect;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

import java.io.File;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.Callable;

@Command(name = "validate", mixinStandardHelpOptions = true, version = "graph 0.1",
        description = "Validates the candidate COBOL code")
public class ValidateCommand implements Callable<Integer> {
    @Option(names = {"-dp", "--dialectJarPath"},
            defaultValue = "che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
            description = "Path to dialect .JAR")
    private String dialectJarPath;

    @Option(names = {"-s", "--srcDir"},
            required = true,
            description = "The Cobol source directory")
    private String sourceDir;

    @CommandLine.Parameters(index = "0..*",
            description = "The programs to analyse", split = " ")
    private List<String> programNames;

    @Option(names = {"-cp", "--copyBooksDir"},
            required = true,
            description = "Copybook directories (repeatable)", split = ",")
    private List<String> copyBookDirs;

    @Option(names = {"-d", "--dialect"},
            defaultValue = "COBOL",
            description = "The COBOL dialect (COBOL, IDMS)")
    private String dialect;

    @Option(names = {"-o", "--output"},
            description = "Validation results output path")
    private String outputPath;

    @Option(names = {"-t", "--strict"},
            description = "Force strict validation, verify all variable usages are valid")
    private boolean isStrict;

    @Override
    public Integer call() {
        List<File> copyBookPaths = copyBookDirs.stream().map(c -> Paths.get(c).toAbsolutePath().toFile()).toList();
        System.out.printf("Running validation in %s mode...%n", isStrict ? "STRICT" : "PERMISSIVE");
        return new ValidateTaskRunner().processPrograms(programNames, sourceDir,
                LanguageDialect.valueOf(dialect), copyBookPaths, dialectJarPath, outputPath, isStrict ? ProgramValidationErrors::IS_COMPLETE_SUCCESS : ProgramValidationErrors::IS_PARTIAL_SUCCESS) ? 0 : 1;
    }
}
