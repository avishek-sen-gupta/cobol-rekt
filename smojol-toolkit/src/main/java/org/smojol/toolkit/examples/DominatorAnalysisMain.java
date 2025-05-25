package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.task.PruneUnreachableTask;
import com.mojo.algorithms.task.ReducibleFlowgraphTestTask;
import com.mojo.algorithms.domain.TranspilerFlowgraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.dialect.LanguageDialect;
import com.mojo.algorithms.visualisation.FlowchartOutputFormat;
import com.mojo.algorithms.id.UUIDProvider;
import org.smojol.common.logging.LoggingConfig;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultOK;
import com.mojo.algorithms.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import static com.mojo.algorithms.task.CommandLineAnalysisTask.BUILD_BASE_ANALYSIS;

public class DominatorAnalysisMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        LoggingConfig.setupLogging();
//        String programName = "flowgraph.cbl";
        String programName = "simple-nonreducible-perform.cbl";
        UUIDProvider idProvider = new UUIDProvider();
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.MERMAID, idProvider), idProvider, new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(BUILD_BASE_ANALYSIS, CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH), ImmutableList.of(programName));
        System.out.println("DONE");
        List<AnalysisTaskResult> results = result.get(programName);
        TranspilerFlowgraph transpilerFlowgraph = ((AnalysisTaskResultOK) results.get(1)).getDetail();
        PruneUnreachableTask.pruneUnreachableInstructions(transpilerFlowgraph);
        System.out.println("Number of nodes = " + transpilerFlowgraph.instructionFlowgraph().vertexSet().size());
//        DepthFirstSpanningTree<TranspilerInstruction, DefaultEdge> spanningTree = new DepthFirstTraversalLabelTask<>(transpilerFlowgraph.instructions().getFirst(), transpilerFlowgraph.instructionFlowgraph(), DefaultEdge.class).run();
//        DepthFirstSpanningTree<BasicBlock<TranspilerInstruction>, DefaultEdge> blockSpanningTree = new DepthFirstTraversalLabelTask<>(transpilerFlowgraph.basicBlocks().getFirst(), transpilerFlowgraph.basicBlockFlowgraph(), DefaultEdge.class).run();
//        List<Pair<BasicBlock<TranspilerInstruction>, BasicBlock<TranspilerInstruction>>> immediateDominators = new BuildDominatorsTask<BasicBlock<TranspilerInstruction>, DefaultEdge>().immediateDominators(blockSpanningTree);
//        Map<BasicBlock<TranspilerInstruction>, Set<BasicBlock<TranspilerInstruction>>> allDominators = new BuildDominatorsTask<BasicBlock<TranspilerInstruction>, DefaultEdge>().allDominators(blockSpanningTree.preOrder(), blockSpanningTree.sourceGraph());
//        DominatorTree<BasicBlock<TranspilerInstruction>, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, blockSpanningTree.sourceGraphRoot(), DefaultEdge.class).run();
//        DJTree<BasicBlock<TranspilerInstruction>, DefaultEdge> djTree = new BuildDJTreeTask<>(dominatorTree, blockSpanningTree, allDominators).run();
        boolean isIrreducible = new ReducibleFlowgraphTestTask<>(transpilerFlowgraph.instructions().getFirst(), transpilerFlowgraph.instructionFlowgraph(), DefaultEdge.class).run();

//        String draw = new MermaidGraph<BasicBlock<TranspilerInstruction>, DefaultEdge>().draw(djTree.graph());
    }
}
