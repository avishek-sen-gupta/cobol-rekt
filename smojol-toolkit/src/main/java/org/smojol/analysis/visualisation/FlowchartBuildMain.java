package org.smojol.analysis.visualisation;

import com.google.common.collect.ImmutableList;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.pipeline.CodeTaskRunner;
import org.smojol.common.id.UUIDProvider;
import org.smojol.interpreter.FlowchartGenerationStrategy;

import java.io.File;
import java.io.IOException;

import static org.smojol.analysis.pipeline.AnalysisTask.*;

public class FlowchartBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.COBOL, FlowchartGenerationStrategy.FULL_PROGRAM, new UUIDProvider())
                .generateForPrograms(ImmutableList.of(
                        WRITE_RAW_AST,
                        WRITE_FLOW_AST
                ), ImmutableList.of("if-test.cbl"));
    }
}
