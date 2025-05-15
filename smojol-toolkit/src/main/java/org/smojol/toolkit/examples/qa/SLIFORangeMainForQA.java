package org.smojol.toolkit.examples.qa;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.id.UUIDProvider;
import com.mojo.algorithms.domain.InvokingProcedureRange;
import com.mojo.algorithms.domain.TranspilerFlowgraph;
import org.apache.commons.lang3.tuple.Pair;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.resource.LocalFilesystemOperations;
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
import java.util.Set;

import static com.mojo.algorithms.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;
import static com.mojo.algorithms.task.CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH;

public class SLIFORangeMainForQA {
    public static void main(String[] args) throws IOException, InterruptedException {
        String programName = "test-irreducible-simplified.cbl";
        UUIDProvider idProvider = new UUIDProvider();
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/qa-codebase/Missing Programs",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/qa-codebase/Missing Programs")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.COBOL, new FullProgram(FlowchartOutputFormat.MERMAID, idProvider), idProvider, new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS, BUILD_TRANSPILER_FLOWGRAPH), ImmutableList.of(programName));
        List<AnalysisTaskResult> results = result.get(programName);
        TranspilerFlowgraph transpilerFlowgraph = ((AnalysisTaskResultOK) results.get(1)).getDetail();
        Pair<Set<InvokingProcedureRange>, Set<InvokingProcedureRange>> categorisedRanges = transpilerFlowgraph.categorisedRanges();
        Set<InvokingProcedureRange> allSLIFORanges = categorisedRanges.getLeft();
        System.out.println("SLIFO Ranges\n----------------------");
        allSLIFORanges.forEach(range -> System.out.println(range.range()));
        Set<InvokingProcedureRange> nonSLIFORanges = categorisedRanges.getRight();
        System.out.println("Non-SLIFO Ranges\n----------------------");
        nonSLIFORanges.forEach(range -> System.out.println(range.range()));
        System.out.println("Total ranges = " + (categorisedRanges.getLeft().size() + categorisedRanges.getRight().size()));
        System.out.println("SLIFO ranges = " + allSLIFORanges.size());
        System.out.println("Non-SLIFO ranges = " + nonSLIFORanges.size());

        System.out.println("DONE");
    }
}
