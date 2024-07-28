package org.smojol.analysis.visualisation;

import com.google.common.collect.ImmutableList;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.pipeline.GraphDBTasks;
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

import static org.smojol.analysis.pipeline.AnalysisTask.*;

public class FlowchartBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        String dialectJarPath = "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar";
        new GraphDBTasks("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                new File[]{new File("/Users/asgupta/code/smojol/smojol-test-code")},
                dialectJarPath, LanguageDialect.COBOL, FlowchartGenerationStrategy.FULL_PROGRAM)
                .generateForPrograms(ImmutableList.of("if-test.cbl"), ImmutableList.of(
                        WRITE_RAW_AST,
                        WRITE_FLOW_AST
                ));
    }
}
