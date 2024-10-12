package org.smojol.toolkit.analysis.task.transpiler;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector;
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm;
import org.jgrapht.graph.AsSubgraph;
import org.jgrapht.graph.DefaultEdge;
import org.smojol.common.flowchart.MermaidGraph;
import org.smojol.common.graph.*;
import org.smojol.common.id.Identifiable;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.smojol.common.graph.GraphOperations.cloneGraph;

/*
Based on "Identifying Loops Using DJ Graphs" by Sreedhar-Gao-Lee (1996)
 */
public class LoopBodyDetectionTask<V extends Identifiable, E> {

    private final V sourceGraphRoot;
    private final Graph<V, E> sourceGraph;
    private final Class<E> edgeClass;

    public LoopBodyDetectionTask(V sourceGraphRoot, Graph<V, E> sourceGraph, Class<E> edgeClass) {
        this.sourceGraphRoot = sourceGraphRoot;
        this.sourceGraph = sourceGraph;
        this.edgeClass = edgeClass;
    }

    public Pair<Set<Set<V>>, Set<Set<V>>> run() {
        DepthFirstSearchOrderingTask<V, E> dfsTask = new DepthFirstSearchOrderingTask<>(sourceGraphRoot, sourceGraph, edgeClass);
        DepthFirstSpanningTree<V, E> spanningTree = dfsTask.run();

        List<Pair<V, V>> immediateDominators = new BuildDominatorsTask<V, E>().immediateDominators(spanningTree);
        Map<V, Set<V>> allDominators = new BuildDominatorsTask<V, E>().allDominators(spanningTree.preOrdered(), sourceGraph);
        DominatorTree<V, E> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), edgeClass).run();
        DJTree<V, E> djTree = new BuildDJTreeTask<>(dominatorTree, spanningTree, allDominators, edgeClass).run();
        Graph<V, E> djGraph = djTree.graph();
        DepthFirstSearchOrderingTask<V, E> dfsTaskOnDJTree = new DepthFirstSearchOrderingTask<>(djTree.root(), djGraph, edgeClass);
        DepthFirstSpanningTree<V, E> djSpanningTree = dfsTaskOnDJTree.run();
        ClassifiedEdges<E> classifiedEdges = djSpanningTree.classifiedEdges();
        Map<Integer, Set<V>> dominatorLevelMap = djTree.dominatorLevels();
        Set<E> backEdges = classifiedEdges.backEdges();
        Set<E> mutableBackEdges = new HashSet<>(backEdges);
//        backEdges.forEach(be -> {
//            if (djGraph.getEdgeSource(be).id().equals("9") && djGraph.getEdgeTarget(be).id().equals("5")) {
//                throw new RuntimeException("THIS IS NOT POSSIBLE");
//            }
//        });
        Set<Integer> depths = dominatorLevelMap.keySet();
        int maxTreeDepth = depths.stream().max((o1, o2) -> {
            if (o1.equals(o2)) return 0;
            return o1 < o2 ? -1 : 1;
        }).get();

        Graph<V, E> collapsibleDJGraph = cloneGraph(djGraph, edgeClass);
        Set<Set<V>> allReducibleLoopBodies = new HashSet<>();
        Set<Set<V>> allIrreducibleLoopBodies = new HashSet<>();
        for (int treeDepth = maxTreeDepth; treeDepth >= 0; treeDepth--) {
            String startOfIterationState = new MermaidGraph<V, E>().draw(collapsibleDJGraph);
            Set<V> nodesAtTreeDepth = dominatorLevelMap.get(treeDepth).stream().filter(node -> collapsibleDJGraph.vertexSet().contains(node)).collect(Collectors.toUnmodifiableSet());
            Set<E> backEdgesAtTreeDepth = nodesAtTreeDepth.stream().flatMap(node -> collapsibleDJGraph.incomingEdgesOf(node).stream().filter(mutableBackEdges::contains)).collect(Collectors.toUnmodifiableSet());
            Set<E> crossJoinEdges = backEdgesAtTreeDepth.stream().filter(be -> be instanceof CrossJoinEdge).collect(Collectors.toUnmodifiableSet());
            Sets.SetView<E> backJoinEdges = Sets.difference(backEdgesAtTreeDepth, crossJoinEdges);
            Set<Pair<V, Set<V>>> reducibleLoopBodies = backJoinEdges.stream().map(bje -> ImmutablePair.of(collapsibleDJGraph.getEdgeTarget(bje), new NaturalLoopOfBackEdgeTask<>(bje, collapsibleDJGraph).run())).collect(Collectors.toUnmodifiableSet());
            reducibleLoopBodies.forEach(loopBody -> collapse(loopBody, collapsibleDJGraph, mutableBackEdges));
            allReducibleLoopBodies.addAll(reducibleLoopBodies.stream().map(Pair::getRight).toList());
            if (crossJoinEdges.isEmpty()) {
                String endOfIterationState = new MermaidGraph<V, E>().draw(collapsibleDJGraph);
                continue;
            }
            int x = treeDepth;
            Set<Set<V>> irreducibleLoopBodies = crossJoinEdges.stream().flatMap(cje -> {
                Set<V> nodesAtOrGreaterDepth = dominatorLevelMap.entrySet().stream()
                        .filter(e -> e.getKey() >= x)
                        .flatMap(e -> e.getValue().stream())
                        .filter(node -> collapsibleDJGraph.vertexSet().contains(node))
                        .collect(Collectors.toUnmodifiableSet());
                Graph<V, E> inducedSubgraph = new AsSubgraph<>(collapsibleDJGraph, nodesAtOrGreaterDepth);
                List<Graph<V, E>> stronglyConnectedComponents = stronglyConnectedComponents(inducedSubgraph).stream().filter(scc -> scc.vertexSet().size() > 1).toList();
                stronglyConnectedComponents.forEach(scc -> collapse(ImmutablePair.of(collapsibleDJGraph.getEdgeTarget(cje), scc.vertexSet()), collapsibleDJGraph, mutableBackEdges));
                String endOfIterationState = new MermaidGraph<V, E>().draw(collapsibleDJGraph);
                return stronglyConnectedComponents.stream().map(Graph::vertexSet);
            }).collect(Collectors.toUnmodifiableSet());
            allIrreducibleLoopBodies.addAll(irreducibleLoopBodies);
            System.out.println("Well");
            System.out.println();
        }
        return ImmutablePair.of(allReducibleLoopBodies, allIrreducibleLoopBodies);
    }

    private void collapse(Pair<V, Set<V>> reducibleLoop, Graph<V, E> collapsibleDJGraph, Set<E> mutableBackEdges) {
        V loopHeader = reducibleLoop.getLeft();
        Set<V> loopBody = reducibleLoop.getRight();
        System.out.println("Merging " + String.join(",", loopBody.stream().map(Identifiable::id).toList()));
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
