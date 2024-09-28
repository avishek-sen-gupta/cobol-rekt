package org.smojol.toolkit.examples;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector;
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartOutputFormat;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.logging.LoggingConfig;
import org.smojol.common.resource.LocalFilesystemOperations;
import org.smojol.common.transpiler.TranspilerInstruction;
import org.smojol.common.transpiler.TranspilerModel;
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
import java.util.Set;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.truncate;

public class ImproperSCCsMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        LoggingConfig.setupLogging();
        String programName = "simple-nonreducible-perform.cbl";
        Map<String, List<AnalysisTaskResult>> result = new CodeTaskRunner("/Users/asgupta/code/smojol/smojol-test-code",
                "/Users/asgupta/code/smojol/out/report",
                ImmutableList.of(new File("/Users/asgupta/code/smojol/smojol-test-code")),
                "/Users/asgupta/code/smojol/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar",
                LanguageDialect.IDMS, new FullProgram(FlowchartOutputFormat.MERMAID), new UUIDProvider(), new OccursIgnoringFormat1DataStructureBuilder(), new ProgramSearch(), new LocalFilesystemOperations())
                .runForPrograms(ImmutableList.of(CommandLineAnalysisTask.BUILD_TRANSPILER_MODEL), ImmutableList.of(programName));
        System.out.println("DONE");
        List<AnalysisTaskResult> results = result.get(programName);
        TranspilerModel model = ((AnalysisTaskResultOK) results.getFirst()).getDetail();
        System.out.println("Number of nodes = " + model.jgraph().vertexSet().size());

        model.pruneUnreachables();
        Graph<TranspilerInstruction, DefaultEdge> originalGraph = model.jgraph();
        StrongConnectivityAlgorithm<TranspilerInstruction, DefaultEdge> scAlg = new KosarajuStrongConnectivityInspector<>(originalGraph);
        List<Graph<TranspilerInstruction, DefaultEdge>> stronglyConnectedComponents = scAlg.getStronglyConnectedComponents();

        List<Pair<Graph<TranspilerInstruction, DefaultEdge>, Set<DefaultEdge>>> sccIncomingEdgePairs = stronglyConnectedComponents.stream().map(scc -> {
            Set<DefaultEdge> allIncomingEdges = scc.vertexSet().stream().flatMap(v -> originalGraph.incomingEdgesOf(v).stream()).collect(Collectors.toUnmodifiableSet());
            Set<DefaultEdge> externalEdgesIntoSCC = Sets.difference(allIncomingEdges, scc.edgeSet());
            return Pair.of(scc, externalEdgesIntoSCC);
        }).toList();

        List<Pair<Graph<TranspilerInstruction, DefaultEdge>, Set<DefaultEdge>>> sccMultipleEdgePairs = sccIncomingEdgePairs.stream().filter(p -> p.getRight().stream().map(originalGraph::getEdgeTarget).collect(Collectors.toUnmodifiableSet()).size() > 1).toList();
        System.out.println("Number of improper SCCs = " + sccMultipleEdgePairs.size());
        sccMultipleEdgePairs.forEach(scc -> System.out.printf("SCC [%s]%nEntries are: %s%n", String.join(",", nodeDescriptions(scc.getLeft().vertexSet())), nodeDescriptions(scc.getRight().stream().map(originalGraph::getEdgeSource).collect(Collectors.toUnmodifiableSet()))));
    }

    private static List<String> nodeDescriptions(Set<TranspilerInstruction> vertices) {
        return vertices.stream().map(v -> truncate(v.sentinel() + " -> " + v.ref().description(), 100)).toList();
    }
}
