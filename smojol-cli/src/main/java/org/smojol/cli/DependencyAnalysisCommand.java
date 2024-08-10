package org.smojol.cli;

import org.smojol.analysis.pipeline.AnalyseDependenciesTask;
import org.smojol.analysis.visualisation.CobolProgram;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.Callable;

@Command(name = "dependency", mixinStandardHelpOptions = true, version = "graph 0.1",
        description = "Implements various operations useful for reverse engineering Cobol code")
public class DependencyAnalysisCommand implements Callable<Integer> {
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

    @Override
    public Integer call() throws IOException {
        List<File> copyBookPaths = copyBookDirs.stream().map(c -> Paths.get(c).toAbsolutePath().toFile()).toList();
        copyBookPaths.forEach(cpp -> System.out.println(cpp.getAbsolutePath()));
        CobolProgram root = new AnalyseDependenciesTask(sourceDir, copyBookPaths, reportRootDir, dialectJarPath).run(programName);
        return 0;
    }
}


