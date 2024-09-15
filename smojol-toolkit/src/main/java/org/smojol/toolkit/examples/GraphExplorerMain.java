package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.defined.CodeTaskRunner;
import org.smojol.common.id.UUIDProvider;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;

import static org.smojol.common.flowchart.FlowchartOutputFormat.PNG;

public class GraphExplorerMain {
    public static void main(String[] args) throws IOException {
        String dialectJarPath = "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar";
        new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                dialectJarPath, LanguageDialect.COBOL   , new FullProgram(PNG), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(
                        CommandLineAnalysisTask.INJECT_INTO_NEO4J,
                        CommandLineAnalysisTask.EXPORT_TO_GRAPHML,
                        CommandLineAnalysisTask.WRITE_FLOW_AST
                ), ImmutableList.of("test-exp.cbl"));
    }
}
