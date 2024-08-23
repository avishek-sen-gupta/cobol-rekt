package org.smojol.toolkit.analysis.visualisation;

import com.google.common.collect.ImmutableList;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.toolkit.analysis.pipeline.CodeTaskRunner;
import org.smojol.toolkit.analysis.pipeline.CommandLineAnalysisTask;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;

public class AwsCardDemoFlowchartBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        new CodeTaskRunner("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cbl",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy"),
                        new File("/Users/asgupta/code/aws-mainframe-modernization-carddemo/app/cpy-bms")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.COBOL, new FullProgram(FlowchartOutputFormat.SVG), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder())
                .runForPrograms(ImmutableList.of(CommandLineAnalysisTask.EXPORT_UNIFIED_TO_JSON), ImmutableList.of("CSUTLDTC.cbl"));
    }
}
