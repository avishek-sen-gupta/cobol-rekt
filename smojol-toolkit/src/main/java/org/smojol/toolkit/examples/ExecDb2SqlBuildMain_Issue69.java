package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import com.mojo.algorithms.id.UUIDProvider;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisModel;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultOK;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import static com.mojo.algorithms.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;
import static com.mojo.algorithms.task.CommandLineAnalysisTask.WRITE_RAW_AST;

public class ExecDb2SqlBuildMain_Issue69 {
    public static void main(String[] args) throws IOException, InterruptedException {
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.COBOL, new FullProgram(FlowchartOutputFormat.PNG, new UUIDProvider()),
                new UUIDProvider(),
                new OccursIgnoringFormat1DataStructureBuilder(),
                new ProgramSearch(),
                new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS, WRITE_RAW_AST), ImmutableList.of("exec-db2-sql-issue-69.cbl"));
        AnalysisTaskResultOK ok = (AnalysisTaskResultOK) result.values().stream().toList().getFirst().getFirst();
        BaseAnalysisModel base = ok.getDetail();
        System.out.println("DONE");
    }
}
