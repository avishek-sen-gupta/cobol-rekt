package com.mojo.algorithms.task;

import com.google.common.collect.Sets;
import com.mojo.algorithms.id.Identifiable;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector;
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm;

import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.truncate;

public class IrreducibleStronglyConnectedComponentsTask<V extends Identifiable, E> {
    private static final Logger LOGGER = Logger.getLogger(IrreducibleStronglyConnectedComponentsTask.class.getName());
    private final Graph<V, E> flowgraph;

    public IrreducibleStronglyConnectedComponentsTask(Graph<V, E> flowgraph) {
        this.flowgraph = flowgraph;
    }

    public AnalysisTaskResult run() {
        StrongConnectivityAlgorithm<V, E> scAlg = new KosarajuStrongConnectivityInspector<>(flowgraph);
        List<Graph<V, E>> stronglyConnectedComponents = scAlg.getStronglyConnectedComponents();

        List<Pair<Graph<V, E>, Set<E>>> sccIncomingEdgePairs = stronglyConnectedComponents.stream().map(scc -> {
            Set<E> allIncomingEdges = scc.vertexSet().stream().flatMap(v -> flowgraph.incomingEdgesOf(v).stream()).collect(Collectors.toUnmodifiableSet());
            Set<E> externalEdgesIntoSCC = Sets.difference(allIncomingEdges, scc.edgeSet());
            return Pair.of(scc, externalEdgesIntoSCC);
        }).toList();

        System.out.println("Number of SCCs = " + sccIncomingEdgePairs.size());
        for (Pair<Graph<V, E>, Set<E>> sccIncomingEdgePair : sccIncomingEdgePairs) {
            System.out.println("SCC has " + sccIncomingEdgePair.getLeft().vertexSet().size() + " vertices");
        }

        List<Pair<Graph<V, E>, Set<E>>> sccMultipleEdgePairs = sccIncomingEdgePairs.stream().filter(p -> p.getRight().stream().map(flowgraph::getEdgeTarget).collect(Collectors.toUnmodifiableSet()).size() > 1).toList();
        System.out.println("Number of improper SCCs = " + sccMultipleEdgePairs.size());

        sccMultipleEdgePairs.forEach(scc -> System.out.printf("SCC [%s]%n----------------------%nEntries are: %s%n======================%n", String.join(",", edgeDescriptions(scc.getLeft().edgeSet(), flowgraph)), nodeDescriptions(scc.getRight().stream().map(flowgraph::getEdgeSource).collect(Collectors.toUnmodifiableSet()))));

        return AnalysisTaskResult.OK("IRREDUCIBLE_REGIONS_TASK", sccMultipleEdgePairs);
    }

    private List<String> nodeDescriptions(Set<V> vertices) {
        return vertices.stream().map(v -> truncate(v.toString(), 100)).toList();
    }

    private String edgeDescriptions(Set<E> edges, Graph<V, E> graph) {
        return String.join(",", edges.stream().map(e -> String.format("(%s - %s)", graph.getEdgeSource(e).id(), graph.getEdgeTarget(e).id())).toList());
    }
}
