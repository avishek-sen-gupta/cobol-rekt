package org.smojol.toolkit.examples;

import static com.mojo.algorithms.task.CommandLineAnalysisTask.*;
import static com.mojo.algorithms.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.id.UUIDProvider;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.visualisation.FlowchartOutputFormat;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

public class FlowchartBuildMain {
  public static void main(String[] args) throws IOException, InterruptedException {
    UUIDProvider idProvider = new UUIDProvider();
    Map<String, List<AnalysisTaskResult>> result =
        new CodeTaskRunner(
                "/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS,
                new FullProgram(FlowchartOutputFormat.PNG, idProvider),
                idProvider,
                new OccursIgnoringFormat1DataStructureBuilder(),
                new ProgramSearch(),
                new LocalFilesystemOperations())
            .runForPrograms(
                ImmutableList.of(BUILD_BASE_ANALYSIS, DRAW_FLOWCHART),
                ImmutableList.of("simple-if.cbl"));
    System.out.println("DONE");
  }
}
