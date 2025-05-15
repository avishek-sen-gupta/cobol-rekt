package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import com.mojo.algorithms.id.UUIDProvider;
import org.smojol.common.logging.LoggingConfig;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.CommandLineAnalysisTask;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import static com.mojo.algorithms.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class AwsCardDemoTasksMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        LoggingConfig.setupLogging();
        UUIDProvider idProvider = new UUIDProvider();
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cbl",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy"),
                        new File("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy-bms")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.COBOL, new FullProgram(FlowchartOutputFormat.SVG, idProvider), idProvider, new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS, CommandLineAnalysisTask.FLOW_TO_GRAPHML), ImmutableList.of("CBACT01C.cbl"));
        for (Map.Entry<String, List<AnalysisTaskResult>> entry : result.entrySet()) {
            System.out.println(entry.getKey() + ": ");
            for (AnalysisTaskResult taskResult : entry.getValue()) {
                System.out.println(taskResult.toString());
            }
        }
        System.out.println("DONE");
    }
}
