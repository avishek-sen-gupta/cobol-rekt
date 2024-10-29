package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.flowchart.MermaidGraph;
import org.smojol.common.graph.GraphSlice;
import org.smojol.common.graph.GraphSliceTask;
import org.smojol.common.graph.ReachingConditionDefinitionTask;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.pseudocode.CodeSentinelType;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.transpiler.*;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisModel;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.analysis.task.transpiler.BuildTranspilerFlowgraphTask;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

public class ReachingConditionBuildMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.PNG), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(), ImmutableList.of("reaching-condition-test.cbl"));

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
