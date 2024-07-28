package org.smojol.analysis.graph;

import com.google.common.collect.ImmutableList;
import org.smojol.analysis.LanguageDialect;
import org.smojol.analysis.pipeline.CodeTaskRunner;
import org.smojol.common.id.UUIDProvider;
import org.smojol.interpreter.FlowchartGenerationStrategy;

import java.io.File;
import java.io.IOException;

import static org.smojol.analysis.pipeline.AnalysisTask.*;

public class GraphExplorerMain {
    public static void main(String[] args) throws IOException {
        String dialectJarPath = "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar";
        new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                dialectJarPath, LanguageDialect.COBOL   , FlowchartGenerationStrategy.FULL_PROGRAM, new UUIDProvider())
                .generateForPrograms(ImmutableList.of("test-exp.cbl"), ImmutableList.of(
                        INJECT_INTO_NEO4J,
                        EXPORT_TO_GRAPHML,
                        WRITE_FLOW_AST
                ));
    }
}
