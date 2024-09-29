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

    public AnalysisTaskResult run(Graph<V, E> graph) {
        StrongConnectivityAlgorithm<V, E> scAlg = new KosarajuStrongConnectivityInspector<>(graph);
        List<Graph<V, E>> stronglyConnectedComponents = scAlg.getStronglyConnectedComponents();

        List<Pair<Graph<V, E>, Set<E>>> sccIncomingEdgePairs = stronglyConnectedComponents.stream().map(scc -> {
            Set<E> allIncomingEdges = scc.vertexSet().stream().flatMap(v -> graph.incomingEdgesOf(v).stream()).collect(Collectors.toUnmodifiableSet());
            Set<E> externalEdgesIntoSCC = Sets.difference(allIncomingEdges, scc.edgeSet());
            return Pair.of(scc, externalEdgesIntoSCC);
        }).toList();

        List<Pair<Graph<V, E>, Set<E>>> sccMultipleEdgePairs = sccIncomingEdgePairs.stream().filter(p -> p.getRight().stream().map(graph::getEdgeTarget).collect(Collectors.toUnmodifiableSet()).size() > 1).toList();
        System.out.println("Number of improper SCCs = " + sccMultipleEdgePairs.size());

        sccMultipleEdgePairs.forEach(scc -> System.out.printf("SCC [%s]%n----------------------%nEntries are: %s%n======================%n", String.join(",", edgeDescriptions(scc.getLeft().edgeSet(), graph)), nodeDescriptions(scc.getRight().stream().map(graph::getEdgeSource).collect(Collectors.toUnmodifiableSet()))));

        boolean reducible = isReducible(stronglyConnectedComponents, graph);
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

    private List<String> nodeDescriptions(Set<V> vertices) {
        return vertices.stream().map(v -> truncate(v.toString(), 100)).toList();
    }

    private String edgeDescriptions(Set<E> edges, Graph<V, E> graph) {
        return String.join(",", edges.stream().map(e -> String.format("(%s - %s)", graph.getEdgeSource(e).id(), graph.getEdgeTarget(e).id())).toList());
    }
}
