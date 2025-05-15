package com.mojo.algorithms.task;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;
import com.mojo.algorithms.domain.*;
import com.mojo.algorithms.id.Identifiable;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector;
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm;
import org.jgrapht.graph.AsSubgraph;
import org.jgrapht.graph.DefaultEdge;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import static com.mojo.algorithms.domain.GraphOperations.cloneGraph;


/*
Based on "Identifying Loops Using DJ Graphs" by Sreedhar-Gao-Lee (1996)
 */
public class LoopBodyDetectionTask<V extends Identifiable, E> {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(LoopBodyDetectionTask.class.getName());

    private final V sourceGraphRoot;
    private final Graph<V, E> sourceGraph;
    private final Class<E> edgeClass;
    private final Function<E, E> cloneEdge;

    public LoopBodyDetectionTask(V sourceGraphRoot, Graph<V, E> sourceGraph, Class<E> edgeClass, Function<E, E> cloneEdge) {
        this.sourceGraphRoot = sourceGraphRoot;
        this.sourceGraph = sourceGraph;
        this.edgeClass = edgeClass;
        this.cloneEdge = cloneEdge;
    }

    public Pair<Set<NaturalLoopBody<V>>, Set<NaturalLoopBody<V>>> run() {
        DepthFirstSearchOrderingTask<V, E> dfsTask = new DepthFirstSearchOrderingTask<>(sourceGraphRoot, sourceGraph, edgeClass);
        DepthFirstSpanningTree<V, E> spanningTree = dfsTask.run();

        LOGGER.info("Building Immediate Dominators...");
        List<Pair<V, V>> immediateDominators = new BuildDominatorsTask<V, E>().immediateDominators(spanningTree);
        LOGGER.info("Building All Dominators...");
        Map<V, Set<V>> allDominators = new BuildDominatorsTask<V, E>().allDominators(spanningTree.preOrdered(), sourceGraph);
        LOGGER.info("Building Dominator Tree...");
        DominatorTree<V, E> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), edgeClass).run();
        LOGGER.info("Building DJ Tree...");
        DJTree<V, E> djTree = new BuildDJTreeTask<>(dominatorTree, spanningTree, allDominators, edgeClass).run();
        Graph<V, E> djGraph = djTree.graph();
        LOGGER.info("Building DFS Task on DJ Tree...");
        DepthFirstSearchOrderingTask<V, E> dfsTaskOnDJTree = new DepthFirstSearchOrderingTask<>(djTree.root(), djGraph, edgeClass);
        DepthFirstSpanningTree<V, E> djSpanningTree = dfsTaskOnDJTree.run();
        ClassifiedEdges<E> classifiedEdges = djSpanningTree.classifiedEdges();
        Map<Integer, Set<V>> dominatorLevelMap = djTree.dominatorLevels();
        Set<E> backEdges = classifiedEdges.backEdges();
        Set<E> mutableBackEdges = new HashSet<>(backEdges);
        Set<Integer> depths = dominatorLevelMap.keySet();
        int maxTreeDepth = depths.stream().max((o1, o2) -> {
            if (o1.equals(o2)) return 0;
            return o1 < o2 ? -1 : 1;
        }).get();

        Graph<V, E> collapsibleDJGraph = cloneGraph(djGraph, edgeClass, cloneEdge);
        Set<NaturalLoopBody<V>> allReducibleLoopBodies = new HashSet<>();
        Set<NaturalLoopBody<V>> allIrreducibleLoopBodies = new HashSet<>();
        for (int treeDepth = maxTreeDepth; treeDepth >= 0; treeDepth--) {
            String startOfIterationState = new MermaidGraph<V, E>().draw(collapsibleDJGraph);
            Set<V> nodesAtTreeDepth = dominatorLevelMap.get(treeDepth).stream().filter(node -> collapsibleDJGraph.vertexSet().contains(node)).collect(Collectors.toUnmodifiableSet());
            Set<E> backEdgesAtTreeDepth = nodesAtTreeDepth.stream().flatMap(node -> collapsibleDJGraph.incomingEdgesOf(node).stream().filter(mutableBackEdges::contains)).collect(Collectors.toUnmodifiableSet());
            Set<E> crossJoinEdges = backEdgesAtTreeDepth.stream().filter(be -> be instanceof CrossJoinEdge).collect(Collectors.toUnmodifiableSet());
            Sets.SetView<E> backJoinEdges = Sets.difference(backEdgesAtTreeDepth, crossJoinEdges);
            Set<NaturalLoopBody<V>> reducibleLoopBodies = backJoinEdges.stream().map(bje -> new NaturalLoopBody<>(collapsibleDJGraph.getEdgeTarget(bje), new NaturalLoopOfBackEdgeTask<>(bje, collapsibleDJGraph).run())).collect(Collectors.toUnmodifiableSet());
            reducibleLoopBodies.forEach(loopBody -> collapse(loopBody, collapsibleDJGraph, mutableBackEdges));
            allReducibleLoopBodies.addAll(reducibleLoopBodies);
            if (crossJoinEdges.isEmpty()) {
                String endOfIterationState = new MermaidGraph<V, E>().draw(collapsibleDJGraph);
                continue;
            }
            int treeDepth_ = treeDepth;
            Set<NaturalLoopBody<V>> irreducibleLoopBodies = crossJoinEdges.stream().flatMap(cje -> {
                Set<V> nodesAtOrGreaterDepth = dominatorLevelMap.entrySet().stream()
                        .filter(e -> e.getKey() >= treeDepth_)
                        .flatMap(e -> e.getValue().stream())
                        .filter(node -> collapsibleDJGraph.vertexSet().contains(node))
                        .collect(Collectors.toUnmodifiableSet());
                Graph<V, E> inducedSubgraph = new AsSubgraph<>(collapsibleDJGraph, nodesAtOrGreaterDepth);
                List<Graph<V, E>> stronglyConnectedComponents = stronglyConnectedComponents(inducedSubgraph).stream().filter(scc -> scc.vertexSet().size() > 1).toList();

                Set<NaturalLoopBody<V>> irreducibleLoopBodies2 = stronglyConnectedComponents.stream().map(scc -> new NaturalLoopBody<>(collapsibleDJGraph.getEdgeTarget(cje), scc.vertexSet())).collect(Collectors.toUnmodifiableSet());
                irreducibleLoopBodies2.forEach(ilb -> collapse(ilb, collapsibleDJGraph, mutableBackEdges));
                String endOfIterationState = new MermaidGraph<V, E>().draw(collapsibleDJGraph);
                return irreducibleLoopBodies2.stream();
            }).collect(Collectors.toUnmodifiableSet());
            allIrreducibleLoopBodies.addAll(irreducibleLoopBodies);
        }
        return ImmutablePair.of(allReducibleLoopBodies, allIrreducibleLoopBodies);
    }

    private void collapse(NaturalLoopBody<V> reducibleLoop, Graph<V, E> collapsibleDJGraph, Set<E> mutableBackEdges) {
        V loopHeader = reducibleLoop.singleLoopHeader();
        Set<V> loopBody = reducibleLoop.loopNodes();
        LOGGER.info("Merging " + String.join(",", loopBody.stream().map(Identifiable::id).toList()));
        if (loopBody.size() <= 1) return;
        Set<E> allOutgoingEdges = loopBody.stream().flatMap(loopNode -> collapsibleDJGraph.outgoingEdgesOf(loopNode).stream()).collect(Collectors.toUnmodifiableSet());
        Set<E> allIncomingEdges = loopBody.stream().flatMap(loopNode -> collapsibleDJGraph.incomingEdgesOf(loopNode).stream()).collect(Collectors.toUnmodifiableSet());
        Set<E> edgesExitingLoop = allOutgoingEdges.stream().filter(oe -> !loopBody.contains(collapsibleDJGraph.getEdgeTarget(oe))).collect(Collectors.toUnmodifiableSet());
        Set<E> edgesEnteringLoop = allIncomingEdges.stream().filter(oe -> !loopBody.contains(collapsibleDJGraph.getEdgeSource(oe))).collect(Collectors.toUnmodifiableSet());

        collapseOutgoing(collapsibleDJGraph, edgesExitingLoop, loopHeader, loopBody, mutableBackEdges);
        collapseIncoming(collapsibleDJGraph, edgesEnteringLoop, loopHeader, loopBody, mutableBackEdges);
        Sets.SetView<V> nodesToRemove = Sets.difference(loopBody, ImmutableSet.of(loopHeader));
        nodesToRemove.forEach(collapsibleDJGraph::removeVertex);
    }

    private void collapseOutgoing(Graph<V, E> collapsibleDJGraph, Set<E> edgesExitingLoop, V loopHeader, Set<V> loopBody, Set<E> mutableBackEdges) {
        edgesExitingLoop.forEach(exitingEdge -> {
            V edgeSource = collapsibleDJGraph.getEdgeSource(exitingEdge);
            if (edgeSource == loopHeader) return;
            E typedEdge = (E) typed(exitingEdge);
            mutableBackEdges.remove(exitingEdge);
            mutableBackEdges.add(typedEdge);
            collapsibleDJGraph.addEdge(loopHeader, collapsibleDJGraph.getEdgeTarget(exitingEdge), typedEdge);
        });
    }

    private void collapseIncoming(Graph<V, E> collapsibleDJGraph, Set<E> edgesEnteringLoop, V loopHeader, Set<V> loopBody, Set<E> mutableBackEdges) {
        edgesEnteringLoop.forEach(enteringEdge -> {
            V edgeTarget = collapsibleDJGraph.getEdgeTarget(enteringEdge);
            if (edgeTarget == loopHeader) return;
            E typedEdge = (E) typed(enteringEdge);
            mutableBackEdges.remove(enteringEdge);
            mutableBackEdges.add(typedEdge);
            collapsibleDJGraph.addEdge(collapsibleDJGraph.getEdgeSource(enteringEdge), loopHeader, typedEdge);
        });
    }

    private DefaultEdge typed(E exitingEdge) {
        return switch (exitingEdge) {
            case DominatorEdge de -> new DominatorEdge();
            case CrossJoinEdge cje -> new CrossJoinEdge();
            case BackJoinEdge bje -> new BackJoinEdge();
            default -> throw new IllegalStateException("Unexpected value: " + exitingEdge);
        };
    }

    private static <V extends Identifiable, E> List<Graph<V, E>> stronglyConnectedComponents(Graph<V, E> inducedSubgraph) {
        StrongConnectivityAlgorithm<V, E> scAlg = new KosarajuStrongConnectivityInspector<>(inducedSubgraph);
        return scAlg.getStronglyConnectedComponents();
    }
}
