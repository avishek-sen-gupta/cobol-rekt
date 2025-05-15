package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.CodeSentinelType;
import com.mojo.algorithms.domain.GraphSlice;
import com.mojo.algorithms.domain.TranspilerFlowgraph;
import com.mojo.algorithms.domain.TranspilerInstruction;
import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.task.*;
import com.mojo.algorithms.domain.MermaidGraph;
import com.mojo.algorithms.id.UUIDProvider;
import com.mojo.algorithms.transpiler.*;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.task.analysis.BuildTranspilerFlowgraphTask;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisModel;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import static com.mojo.algorithms.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class ReachingConditionBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        UUIDProvider idProvider = new UUIDProvider();
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.PNG, idProvider), idProvider, new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS), ImmutableList.of("reaching-condition-test.cbl"));

        AnalysisTaskResult value = result.values().stream().toList().getFirst().getFirst();
        BaseAnalysisModel baseResult = ((AnalysisTaskResultOK) value).getDetail();
        BuildTranspilerFlowgraphTask buildTranspilerFlowgraphTask = new BuildTranspilerFlowgraphTask(baseResult.rawAST(), baseResult.dataStructures(), baseResult.symbolTable(), ImmutableList.of());
        TranspilerFlowgraph transpilerFlowgraph = buildTranspilerFlowgraphTask.run();
        PruneUnreachableTask.pruneUnreachableInstructions(transpilerFlowgraph);
        List<TranspilerInstruction> instructions = transpilerFlowgraph.instructions();
        TranspilerInstruction start = instructions.getFirst();
        TranspilerInstruction last = instructions.getLast();
        String draw = new MermaidGraph<TranspilerInstruction, DefaultEdge>().draw(transpilerFlowgraph.instructionFlowgraph());
        TranspilerInstruction printInstruction = instructions.stream().filter(instr -> instr.ref() instanceof PrintTranspilerNode && instr.sentinel() == CodeSentinelType.BODY).findFirst().get();
        GraphSlice<TranspilerInstruction, DefaultEdge> slice = new GraphSliceTask<>(transpilerFlowgraph.instructionFlowgraph(), DefaultEdge.class).run(start, printInstruction);
        Map<TranspilerInstruction, TranspilerNode> reachingConditions = new ReachingConditionDefinitionTask<>(slice).run();
        reachingConditions.forEach((key, value1) -> System.out.println(key.description() + ": " + value1.description()));
        System.out.println("DONE");
    }
}
