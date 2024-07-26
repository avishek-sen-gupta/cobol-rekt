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

public class FlowchartBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
//        String sourceDir = "/Users/asgupta/code/smojol/smojol-test-code";
//        String sourceDir = "/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cbl";
        String sourceDir = "/Users/asgupta/code/sample-cobol-code/src";
//        File[] copyBookPaths = new File[]{new File("/Users/asgupta/code/smojol/smojol-test-code")};
        File[] copyBookPaths = new File[]{new File("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy")};
        String dialectJarPath = "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar";
        String reportRootDir = "/Users/asgupta/code/smojol/out/report";

//        List<String> programNames = ImmutableList.of("CBACT01C.cbl");
        List<String> programNames = ImmutableList.of("if-test.cbl");
//        List<String> programNames = ImmutableList.of("day-from-date.cbl");
        new FlowchartTasks(sourceDir, reportRootDir, copyBookPaths, dialectJarPath).generateForPrograms(programNames, FlowchartGenerationStrategy.FULL_PROGRAM, LanguageDialect.IDMS);
    }
}
