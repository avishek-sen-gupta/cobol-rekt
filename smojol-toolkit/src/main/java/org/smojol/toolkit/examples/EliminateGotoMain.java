package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.pseudocode.BasicBlock;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.transpiler.*;
import org.smojol.toolkit.analysis.pipeline.ProgramSearch;
import org.smojol.toolkit.analysis.task.analysis.CodeTaskRunner;
import org.smojol.toolkit.analysis.task.transpiler.StructuredProgramTheoremFormTranspilerTask;
import org.smojol.toolkit.interpreter.FullProgram;
import org.smojol.toolkit.interpreter.structure.OccursIgnoringFormat1DataStructureBuilder;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.toolkit.task.AnalysisTaskResultOK;
import org.smojol.toolkit.task.CommandLineAnalysisTask;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

public class EliminateGotoMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        String programName = "simple-goto.cbl";
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.MERMAID), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(CommandLineAnalysisTask.BUILD_TRANSPILER_FLOWGRAPH), ImmutableList.of(programName));
        List<AnalysisTaskResult> results = result.get(programName);
        TranspilerFlowgraph transpilerFlowgraph = ((AnalysisTaskResultOK) results.getFirst()).getDetail();
        TranspilerNode tree = transpilerFlowgraph.transpilerTree();
        JumpTranspilerNode firstGoto = (JumpTranspilerNode) tree.findAllRecursive(n1 -> n1 instanceof JumpTranspilerNode).getFirst();
        TreeSmith treeSmith = new TreeSmith(tree);
        boolean b2 = treeSmith.escapeScope(firstGoto);
        TranspilerNode jumpIf1 = tree.findAllRecursive(n -> n instanceof JumpIfTranspilerNode).getFirst();
        boolean b1 = treeSmith.escapeScope(jumpIf1);
        JumpIfTranspilerNode jumpIf2 = (JumpIfTranspilerNode) tree.findAllRecursive(n -> n instanceof JumpIfTranspilerNode).getFirst();
        boolean b3 = treeSmith.escapeScope(jumpIf2);
        JumpIfTranspilerNode jumpIf3 = (JumpIfTranspilerNode) tree.findAllRecursive(n -> n instanceof JumpIfTranspilerNode).getFirst();
        boolean b = treeSmith.eliminateForwardJump(jumpIf3);
        System.out.println("DONE");
    }
}
