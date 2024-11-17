package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.IncrementingIdProvider;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.transpiler.*;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.analysis.task.transpiler.BuildTranspilerInstructionsFromIntermediateTreeTask;
import org.smojol.toolkit.analysis.task.transpiler.ProcedureBodyTask;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.smojol.toolkit.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class SLIFORangeMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        String programName = "actually-reducible-but-bug-perform.cbl";
        UUIDProvider idProvider = new UUIDProvider();
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.MERMAID, idProvider), idProvider, new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS, CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH), ImmutableList.of(programName));
        List<AnalysisTaskResult> results = result.get(programName);
        TranspilerFlowgraph transpilerFlowgraph = ((AnalysisTaskResultOK) results.get(1)).getDetail();
        TranspilerNode tree = transpilerFlowgraph.transpilerTree();
        List<TranspilerInstruction> instructions = new BuildTranspilerInstructionsFromIntermediateTreeTask(tree, new IncrementingIdProvider()).run();
        Graph<TranspilerInstruction, DefaultEdge> implicitCFG = new BuildImplicitInstructionControlFlowgraphTask(instructions, ImmutableList.of()).run();
        Set<InvokingProcedureRange> rangesWithChildren = new ProcedureBodyTask(tree, instructions, implicitCFG).run();
        SLIFORangeCriterionTask task = new SLIFORangeCriterionTask(rangesWithChildren);
        Pair<Set<InvokingProcedureRange>, Set<InvokingProcedureRange>> categorisedRanges = task.run();
        categorisedRanges.getLeft().forEach(range -> System.out.println(range.range()));
        categorisedRanges.getRight().forEach(range -> System.out.println(range.range()));

        System.out.println("DONE");
    }
}
