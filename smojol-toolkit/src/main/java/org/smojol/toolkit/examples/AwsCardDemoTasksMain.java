package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.toolkit.analysis.defined.CodeTaskRunner;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

public class AwsCardDemoTasksMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cbl",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy"),
                        new File("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy-bms")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.COBOL, new FullProgram(FlowchartOutputFormat.SVG), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch())
                .runForPrograms(ImmutableList.of(CommandLineAnalysisTask.WRITE_RAW_AST,
                                                 CommandLineAnalysisTask.EXPORT_TO_GRAPHML,
                                                 CommandLineAnalysisTask.WRITE_CFG,
                                                 CommandLineAnalysisTask.WRITE_DATA_STRUCTURES,
                                                 CommandLineAnalysisTask.EXPORT_UNIFIED_TO_JSON,
                                                 CommandLineAnalysisTask.INJECT_INTO_NEO4J,
                                                 CommandLineAnalysisTask.BUILD_PROGRAM_DEPENDENCIES
                                                ), ImmutableList.of("COADM01C.cbl"));
        for (Map.Entry<String, List<AnalysisTaskResult>> entry : result.entrySet()) {
            System.out.println(entry.getKey() + ": ");
            for (AnalysisTaskResult taskResult : entry.getValue()) {
                System.out.println(taskResult.toString());
            }
        }
        System.out.println("DONE");
    }
}
