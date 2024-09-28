package org.smojol.toolkit.analysis.defined;

import com.google.common.collect.Sets;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector;
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm;
import org.smojol.common.id.Identifiable;
import org.smojol.toolkit.task.AnalysisTaskResult;

import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.truncate;

public class IrreducibleRegionsTask<V extends Identifiable, E> {
    private static final Logger LOGGER = Logger.getLogger(IrreducibleRegionsTask.class.getName());

//    public AnalysisTaskResult run() {
//        model.pruneUnreachables();
//        Graph<TranspilerInstruction, DefaultEdge> originalGraph = model.jgraph();
//        StrongConnectivityAlgorithm<TranspilerInstruction, DefaultEdge> scAlg = new KosarajuStrongConnectivityInspector<>(originalGraph);
//        List<Graph<TranspilerInstruction, DefaultEdge>> stronglyConnectedComponents = scAlg.getStronglyConnectedComponents();
//
//        List<Pair<Graph<TranspilerInstruction, DefaultEdge>, Set<DefaultEdge>>> sccIncomingEdgePairs = stronglyConnectedComponents.stream().map(scc -> {
//            Set<DefaultEdge> allIncomingEdges = scc.vertexSet().stream().flatMap(v -> originalGraph.incomingEdgesOf(v).stream()).collect(Collectors.toUnmodifiableSet());
//            Set<DefaultEdge> externalEdgesIntoSCC = Sets.difference(allIncomingEdges, scc.edgeSet());
//            return Pair.of(scc, externalEdgesIntoSCC);
//        }).toList();
//
//        List<Pair<Graph<TranspilerInstruction, DefaultEdge>, Set<DefaultEdge>>> sccMultipleEdgePairs = sccIncomingEdgePairs.stream().filter(p -> p.getRight().stream().map(originalGraph::getEdgeTarget).collect(Collectors.toUnmodifiableSet()).size() > 1).toList();
//        System.out.println("Number of improper SCCs = " + sccMultipleEdgePairs.size());
//
//        sccMultipleEdgePairs.forEach(scc -> System.out.printf("SCC [%s]%n----------------------%nEntries are: %s%n======================%n", String.join(",", edgeDescriptions(scc.getLeft().edgeSet(), originalGraph)), nodeDescriptions(scc.getRight().stream().map(originalGraph::getEdgeSource).collect(Collectors.toUnmodifiableSet()))));
//        return AnalysisTaskResult.OK("INTERVAL_ANALYSIS", sccMultipleEdgePairs);
//    }

    public AnalysisTaskResult run(Graph<V, E> originalGraph) {
        StrongConnectivityAlgorithm<V, E> scAlg = new KosarajuStrongConnectivityInspector<>(originalGraph);
        List<Graph<V, E>> stronglyConnectedComponents = scAlg.getStronglyConnectedComponents();

        List<Pair<Graph<V, E>, Set<E>>> sccIncomingEdgePairs = stronglyConnectedComponents.stream().map(scc -> {
            Set<E> allIncomingEdges = scc.vertexSet().stream().flatMap(v -> originalGraph.incomingEdgesOf(v).stream()).collect(Collectors.toUnmodifiableSet());
            Set<E> externalEdgesIntoSCC = Sets.difference(allIncomingEdges, scc.edgeSet());
            return Pair.of(scc, externalEdgesIntoSCC);
        }).toList();

        List<Pair<Graph<V, E>, Set<E>>> sccMultipleEdgePairs = sccIncomingEdgePairs.stream().filter(p -> p.getRight().stream().map(originalGraph::getEdgeTarget).collect(Collectors.toUnmodifiableSet()).size() > 1).toList();
        System.out.println("Number of improper SCCs = " + sccMultipleEdgePairs.size());

        sccMultipleEdgePairs.forEach(scc -> System.out.printf("SCC [%s]%n----------------------%nEntries are: %s%n======================%n", String.join(",", edgeDescriptions2(scc.getLeft().edgeSet(), originalGraph)), nodeDescriptions2(scc.getRight().stream().map(originalGraph::getEdgeSource).collect(Collectors.toUnmodifiableSet()))));

        boolean reducible = isReducible(stronglyConnectedComponents, originalGraph);
        System.out.println("Reducible = " + reducible);

        return AnalysisTaskResult.OK("IRREDUCIBLE_REGIONS_TASK", sccMultipleEdgePairs);
    }

    private boolean isReducible(List<Graph<V, E>> stronglyConnectedComponents, Graph<V, E> graph) {
        for (Graph<V, E> scc : stronglyConnectedComponents) {
            if (hasMultipleEntryPoints(graph, scc)) {
                return false;  // Graph is irreducible if any SCC has multiple entry points
            }
        }
        return true;
    }

    private boolean hasMultipleEntryPoints(Graph<V, E> graph, Graph<V, E> scc) {
        int entryPointCount = 0;
        for (V node : scc.vertexSet()) {
            Set<E> incomingEdges = graph.incomingEdgesOf(node);
            for (E edge : incomingEdges) {
                if (!scc.containsEdge(edge)) {
                    entryPointCount++;
                    if (entryPointCount > 1) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
//    private static List<String> nodeDescriptions(Set<TranspilerInstruction> vertices) {
//        return vertices.stream().map(v -> truncate(v.sentinel() + " -> " + v.id(), 100)).toList();
//    }

    private List<String> nodeDescriptions2(Set<V> vertices) {
        return vertices.stream().map(v -> truncate(v.toString(), 100)).toList();
    }

//    private static String edgeDescriptions(Set<DefaultEdge> edges, Graph<TranspilerInstruction, DefaultEdge> graph) {
//        return String.join(",", edges.stream().map(e -> String.format("(%s - %s)", graph.getEdgeSource(e).id(), graph.getEdgeTarget(e).id())).toList());
//    }

    private String edgeDescriptions2(Set<E> edges, Graph<V, E> graph) {
        return String.join(",", edges.stream().map(e -> String.format("(%s - %s)", graph.getEdgeSource(e).id(), graph.getEdgeTarget(e).id())).toList());
    }
}
