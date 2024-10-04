package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.flowchart.MermaidGraph;
import org.smojol.common.graph.*;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.logging.LoggingConfig;
import org.smojol.common.pseudocode.BasicBlock;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.transpiler.PruneUnreachableTask;
import org.smojol.common.transpiler.TranspilerFlowgraph;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.analysis.task.transpiler.BuildDJTreeTask;
import org.smojol.toolkit.analysis.task.transpiler.BuildDominatorTreeTask;
import org.smojol.toolkit.analysis.task.transpiler.BuildDominatorsTask;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

public class DominatorAnalysisMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        LoggingConfig.setupLogging();
//        String programName = "flowgraph.cbl";
        String programName = "simple-nonreducible-perform.cbl";
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.MERMAID), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH), ImmutableList.of(programName));
        System.out.println("DONE");
        List<AnalysisTaskResult> results = result.get(programName);
        TranspilerFlowgraph transpilerFlowgraph = ((AnalysisTaskResultOK) results.getFirst()).getDetail();
        PruneUnreachableTask.pruneUnreachableInstructions(transpilerFlowgraph);
        System.out.println("Number of nodes = " + transpilerFlowgraph.instructionFlowgraph().vertexSet().size());
        DepthFirstSpanningTree<TranspilerInstruction, DefaultEdge> spanningTree = new DepthFirstTraversalLabelTask<>(transpilerFlowgraph.instructions().getFirst(), transpilerFlowgraph.instructionFlowgraph(), DefaultEdge.class).run();
        DepthFirstSpanningTree<BasicBlock<TranspilerInstruction>, DefaultEdge> blockSpanningTree = new DepthFirstTraversalLabelTask<>(transpilerFlowgraph.basicBlocks().getFirst(), transpilerFlowgraph.basicBlockFlowgraph(), DefaultEdge.class).run();
        List<Pair<BasicBlock<TranspilerInstruction>, BasicBlock<TranspilerInstruction>>> immediateDominators = new BuildDominatorsTask<BasicBlock<TranspilerInstruction>, DefaultEdge>().immediateDominators(blockSpanningTree);
        DominatorTree<BasicBlock<TranspilerInstruction>, DefaultEdge> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, blockSpanningTree.sourceGraphRoot(), DefaultEdge.class).run();
        DJTree<BasicBlock<TranspilerInstruction>> djTree = new BuildDJTreeTask<>(dominatorTree, blockSpanningTree).run();
        String draw = new MermaidGraph<BasicBlock<TranspilerInstruction>, DefaultEdge>().draw(djTree.graph());
    }
}
