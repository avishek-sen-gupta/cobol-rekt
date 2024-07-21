package org.smojol.analysis.visualisation;

import com.google.common.collect.ImmutableList;
import org.smojol.analysis.LanguageDialect;
import org.smojol.flowchart.FlowchartTasks;
import org.smojol.interpreter.FlowchartGenerationStrategy;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.Option;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;

@Command(name = "flowchart", mixinStandardHelpOptions = true, version = "flowchart 0.1",
         description = "Builds the flowcharts")
class FlowchartCommand implements Callable<Integer> {

    private final String codeRootDir = "/Users/asgupta/code/";

    @Parameters(index = "0",
                defaultValue = codeRootDir + "che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                description = "Path to dialect .JAR")
    private String dialectJarPath;

    @Parameters(index = "1",
                defaultValue = codeRootDir + "aws-mainframe-modernization-carddemo/app/cbl",
                description = ".cbl source directory")
    private String sourceDir;

    @Parameters(index = "2..*",
            defaultValue = "CBACT01C.cbl",
            description = "Program names")
    private List<String> programNames;

    // Can be replaced with a File[] (and the later conversion removed) if we skip default arguments.
    @Option(names = { "-cpy", "--copybookdir" },
            defaultValue = codeRootDir + "aws-mainframe-modernization-carddemo/app/cpy",
            description = ".cpy source directory (repeatable)")
    private String[] copyBookDirs;

    @Option(names = { "-o", "--output" }, required = true,
            defaultValue = codeRootDir + "smojol/out/report",
            description = "Output report directory")
    private String reportRootDir;

    @Override
    public Integer call() throws IOException, InterruptedException {
        // Convert the String[] to File[]. See above.
        File[] copyBookPaths = Arrays.stream(copyBookDirs).map(str -> new File(str)).toArray(File[]::new);

        new FlowchartTasks(sourceDir, reportRootDir, copyBookPaths, dialectJarPath).generateForPrograms(programNames, FlowchartGenerationStrategy.FULL_PROGRAM, LanguageDialect.COBOL);

        return 0;
    }
}

public class FlowchartBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        int exitCode = new CommandLine(new FlowchartCommand()).execute(args);
        System.exit(exitCode);
    }
}
