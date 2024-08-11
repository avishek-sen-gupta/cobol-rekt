package org.smojol.analysis.visualisation;

import com.google.common.collect.ImmutableList;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.pipeline.CodeTaskRunner;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.interpreter.FullProgram;
import org.smojol.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;

import static org.smojol.analysis.pipeline.CommandLineAnalysisTask.*;

public class FlowchartBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.SVG), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder())
                .generateForPrograms(ImmutableList.of(COMPARE_CODE), ImmutableList.of("test-exp.cbl"));
    }
}
