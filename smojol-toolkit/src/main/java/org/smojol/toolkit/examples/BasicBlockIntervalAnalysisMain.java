package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.pseudocode.BasicBlock;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.transpiler.FlowgraphReductionResult;
import org.smojol.common.transpiler.PruneUnreachableTask;
import org.smojol.common.transpiler.TranspilerFlowgraph;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.analysis.task.transpiler.T1_T2_IntervalAnalysisTask;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;

import static org.smojol.toolkit.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class BasicBlockIntervalAnalysisMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        String programName = "simple-nonreducible-perform.cbl";
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.MERMAID, new UUIDProvider()), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS, CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH), ImmutableList.of(programName));
        List<AnalysisTaskResult> results = result.get(programName);
        TranspilerFlowgraph transpilerFlowgraph = ((AnalysisTaskResultOK) results.get(1)).getDetail();
        Graph<BasicBlock<TranspilerInstruction>, DefaultEdge> blockGraph = transpilerFlowgraph.basicBlockFlowgraph();
        Function<BasicBlock<TranspilerInstruction>, Boolean> IS_ROOT = block -> block == transpilerFlowgraph.basicBlocks().getFirst();
        BiFunction<BasicBlock<TranspilerInstruction>, BasicBlock<TranspilerInstruction>, DefaultEdge> DEFAULT_EDGE = (b1, b2) -> new DefaultEdge();
        PruneUnreachableTask.pruneUnreachables(blockGraph, IS_ROOT);
        T1_T2_IntervalAnalysisTask<BasicBlock<TranspilerInstruction>, DefaultEdge> reductionTask = new T1_T2_IntervalAnalysisTask<>(blockGraph, IS_ROOT, DEFAULT_EDGE);
        AnalysisTaskResultOK intervalAnalysisResult = (AnalysisTaskResultOK) reductionTask.run();
        FlowgraphReductionResult<BasicBlock<TranspilerInstruction>, DefaultEdge> reductionResult = intervalAnalysisResult.getDetail();
        System.out.println("DONE");
    }
}
