package org.smojol.toolkit.analysis.task;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.domain.GraphSlice;
import com.mojo.algorithms.task.GraphSliceTask;
import com.mojo.algorithms.task.ReachingConditionDefinitionTask;
import com.mojo.algorithms.transpiler.PrintTranspilerNode;
import com.mojo.algorithms.domain.TranspilerFlowgraph;
import com.mojo.algorithms.domain.TranspilerInstruction;
import com.mojo.algorithms.domain.TranspilerNode;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import com.mojo.algorithms.id.UUIDProvider;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.pipeline.BaseAnalysisModel;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.BuildTranspilerFlowgraphTask;
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

public class ReachingConditionTaskRegressionTest {
    @Test
    @Disabled
    public void canFindReachingConditionForSimpleAcyclicGraph() throws IOException {
        UUIDProvider idProvider = new UUIDProvider();
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.PNG, idProvider), idProvider, new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS), ImmutableList.of("simple-if.cbl"));

        AnalysisTaskResult value = result.values().stream().toList().getFirst().getFirst();
        BaseAnalysisModel baseResult = ((AnalysisTaskResultOK) value).getDetail();
        BuildTranspilerFlowgraphTask buildTranspilerFlowgraphTask = new BuildTranspilerFlowgraphTask(baseResult.rawAST(), baseResult.dataStructures(), baseResult.symbolTable(), ImmutableList.of());
        TranspilerFlowgraph transpilerFlowgraph = buildTranspilerFlowgraphTask.run();
        List<TranspilerInstruction> instructions = transpilerFlowgraph.instructions();
        TranspilerInstruction start = instructions.getFirst();
        TranspilerInstruction printInstruction = instructions.stream().filter(instr -> instr.ref() instanceof PrintTranspilerNode).findFirst().get();
        GraphSlice<TranspilerInstruction, DefaultEdge> slice = new GraphSliceTask<>(transpilerFlowgraph.instructionFlowgraph(), DefaultEdge.class).run(start, printInstruction);
        Map<TranspilerInstruction, TranspilerNode> reachingConditions = new ReachingConditionDefinitionTask<>(slice).run();
        System.out.println("DONE");
    }
}
