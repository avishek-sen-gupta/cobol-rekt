package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.resource.LocalFilesystemOperations;
import com.mojo.algorithms.id.UUIDProvider;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import com.mojo.algorithms.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;

import static com.mojo.algorithms.visualisation.FlowchartOutputFormat.PNG;
import static com.mojo.algorithms.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class GraphExplorerMain {
    public static void main(String[] args) throws IOException {
        String dialectJarPath = "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar";
        UUIDProvider idProvider = new UUIDProvider();
        new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                dialectJarPath, LanguageDialect.COBOL   , new FullProgram(PNG, idProvider), idProvider, new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(
                        BUILD_BASE_ANALYSIS,
                        CommandLineAnalysisTask.FLOW_TO_NEO4J,
                        CommandLineAnalysisTask.FLOW_TO_GRAPHML,
                        CommandLineAnalysisTask.WRITE_FLOW_AST
                ), ImmutableList.of("test-exp.cbl"));
    }
}
