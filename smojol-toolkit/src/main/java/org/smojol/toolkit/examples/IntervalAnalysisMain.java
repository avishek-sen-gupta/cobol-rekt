package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.logging.LoggingConfig;
import org.smojol.common.pseudocode.PseudocodeGraph;
import org.smojol.common.pseudocode.PseudocodeInstruction;
import org.smojol.common.transpiler.FlowgraphTransformer;
import org.smojol.toolkit.analysis.defined.CodeTaskRunner;
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

public class IntervalAnalysisMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        LoggingConfig.setupLogging();
        String programName = "stop-run.cbl";
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.MERMAID), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch())
                .runForPrograms(ImmutableList.of(CommandLineAnalysisTask.BUILD_PSEUDOCODE_GRAPH), ImmutableList.of(programName));
        System.out.println("DONE");
        List<AnalysisTaskResult> results = result.get(programName);
        PseudocodeGraph graph = ((AnalysisTaskResultOK) results.getFirst()).getDetail();

        Graph<PseudocodeInstruction, DefaultEdge> jgraph = new DefaultDirectedGraph<>(DefaultEdge.class);
        graph.instructions().forEach(jgraph::addVertex);
        graph.edges().forEach(e -> jgraph.addEdge(e.getFrom(), e.getTo()));
        FlowgraphTransformer<PseudocodeInstruction, DefaultEdge> transformer = new FlowgraphTransformer<>(jgraph, (a, b) -> new DefaultEdge());
        System.out.println(transformer.isReducible());
        System.out.println(transformer.getEvolutions().getFirst());
    }
}
