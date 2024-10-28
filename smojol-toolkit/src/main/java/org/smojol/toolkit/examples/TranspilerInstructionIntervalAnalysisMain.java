package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.flowchart.MermaidGraph;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.logging.LoggingConfig;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.transpiler.FlowgraphReductionResult;
import org.smojol.common.transpiler.PruneUnreachableTask;
import org.smojol.common.transpiler.TranspilerFlowgraph;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisResult;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.analysis.task.transpiler.BuildTranspilerFlowgraphTask;
import org.smojol.toolkit.analysis.task.transpiler.IntervalAnalysisTask;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

public class TranspilerInstructionIntervalAnalysisMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        LoggingConfig.setupLogging();
//        String programName = "simple-nonreducible-perform.cbl";
//        String programName = "simple-nonreducible-perform-with-goto.cbl";
        String programName = "minimal-nonreducible-perform.cbl";
//        String programName = "incorrect-cfg-nonreducible-perform.cbl";
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.MERMAID), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(), ImmutableList.of(programName));
        System.out.println("DONE");
        List<AnalysisTaskResult> results = result.get(programName);
        BaseAnalysisResult baseAnalysisResult = ((AnalysisTaskResultOK) results.getFirst()).getDetail();
        TranspilerFlowgraph transpilerFlowgraph = new BuildTranspilerFlowgraphTask(baseAnalysisResult.rawAST(), baseAnalysisResult.dataStructures(), null,
                ImmutableList.of()).run();
        System.out.println("Number of nodes = " + transpilerFlowgraph.instructionFlowgraph().vertexSet().size());
        String draw = new MermaidGraph<TranspilerInstruction, DefaultEdge>().draw(transpilerFlowgraph.instructionFlowgraph());
        PruneUnreachableTask.pruneUnreachableInstructions(transpilerFlowgraph);
        AnalysisTaskResultOK intervalAnalysisResult = (AnalysisTaskResultOK) new IntervalAnalysisTask<>(transpilerFlowgraph.instructionFlowgraph(), IntervalAnalysisTask.IS_ROOT, IntervalAnalysisTask.NEW_DEFAULT_EDGE).run();
        FlowgraphReductionResult<TranspilerInstruction, DefaultEdge> reductionResult = intervalAnalysisResult.getDetail();
        System.out.println(reductionResult.evolutions().getLast());
        System.out.println(reductionResult.isReducible());
    }
}
