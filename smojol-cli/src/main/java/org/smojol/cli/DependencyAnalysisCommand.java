package org.smojol.cli;

import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import org.smojol.toolkit.analysis.graph.NamespaceQualifier;
import org.smojol.toolkit.analysis.graph.NodeSpecBuilder;
import org.smojol.common.program.CobolProgram;
import org.smojol.toolkit.analysis.pipeline.*;
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

    @Option(names = {"-cp", "--copyBooksDir"},
            required = true,
            description = "Copybook directories (repeatable)", split = ",")
    private List<String> copyBookDirs;

    @Option(names = {"-x", "--export"},
            description = "Export path")
    private String exportPath;

    @Option(names = {"-n", "--neo4j"},
            description = "Export to Neo4J")
    private boolean injectIntoNeo4j;

    @Option(names = {"-d", "--dialect"},
            defaultValue = "COBOL",
            description = "The COBOL dialect (COBOL, IDMS)")
    private String dialect;

    @Override
    public Integer call() throws IOException {
        List<File> copyBookPaths = copyBookDirs.stream().map(c -> Paths.get(c).toAbsolutePath().toFile()).toList();
        copyBookPaths.forEach(cbp -> System.out.println(cbp.getAbsolutePath()));
        AnalysisTaskResult result = new AnalyseProgramDependenciesTask(sourceDir, copyBookPaths, "dummy", dialectJarPath).run(programName);
        CobolProgram root = switch (result) {
            case AnalysisTaskResultOK ok -> (CobolProgram) ok.getDetail();
            case AnalysisTaskResultError e -> throw new RuntimeException(e.getException());
        };
        if (exportPath != null) new ExportProgramDependenciesTask(exportPath).run(root);
        if (injectIntoNeo4j)
            new InjectProgramDependenciesIntoNeo4JTask(new NodeSpecBuilder(new NamespaceQualifier("DEP-GRAPH")),
                    new GraphSDK(new Neo4JDriverBuilder().fromEnv())).run(root);
        return 0;
    }
}
