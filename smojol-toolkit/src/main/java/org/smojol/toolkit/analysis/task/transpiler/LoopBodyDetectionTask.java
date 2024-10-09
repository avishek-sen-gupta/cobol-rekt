package org.smojol.toolkit.analysis.task.transpiler;

import com.google.common.collect.Sets;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector;
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm;
import org.jgrapht.graph.AsSubgraph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.smojol.common.graph.*;
import org.smojol.common.id.Identifiable;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

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
        Map<V, Set<V>> allDominators = new BuildDominatorsTask<V, E>().allDominators(spanningTree.preOrder(), sourceGraph);
        DominatorTree<V, E> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), edgeClass).run();
        DJTree<V, E> djTree = new BuildDJTreeTask<>(dominatorTree, spanningTree, allDominators).run();
        Graph<V, E> djGraph = djTree.graph();
        DepthFirstSearchOrderingTask<V, E> dfsTaskOnDJTree = new DepthFirstSearchOrderingTask<>(djTree.root(), djGraph, edgeClass);
        DepthFirstSpanningTree<V, E> djSpanningTree = dfsTaskOnDJTree.run();
        ClassifiedEdges<E> classifiedEdges = djSpanningTree.classifiedEdges();
        Map<Integer, Set<V>> depthToNodeMap = djSpanningTree.depthToNodeMap();
        Set<E> backEdges = classifiedEdges.backEdges();
//        return backEdges.stream().noneMatch(be -> be.getClass() == CrossJoinEdge.class);
        Set<Integer> depths = spanningTree.depthToNodeMap().keySet();
        int maxTreeDepth = depths.stream().max((o1, o2) -> {
            if (o1.equals(o2)) return 0;
            return o1 < o2 ? 1 : -1;
        }).get();

        Graph<V, E> collapsibleDJGraph = clone(djGraph);
        Set<Set<V>> allReducibleLoopBodies = new HashSet<>();
        Set<Set<V>> allIrreducibleLoopBodies = new HashSet<>();
        for (int treeDepth = maxTreeDepth; treeDepth >= 0; treeDepth--) {
            Set<V> nodesAtTreeDepth = depthToNodeMap.get(treeDepth);
            Set<E> backEdgesAtTreeDepth = nodesAtTreeDepth.stream().flatMap(node -> collapsibleDJGraph.incomingEdgesOf(node).stream().filter(backEdges::contains)).collect(Collectors.toUnmodifiableSet());
            Set<E> crossJoinEdges = backEdgesAtTreeDepth.stream().filter(be -> be instanceof CrossJoinEdge).collect(Collectors.toUnmodifiableSet());
            Sets.SetView<E> backJoinEdges = Sets.difference(backEdgesAtTreeDepth, crossJoinEdges);
            Set<Set<V>> reducibleLoopBodies = backJoinEdges.stream().map(bje -> new NaturalLoopOfBackEdgeTask<>(bje, collapsibleDJGraph).run()).collect(Collectors.toUnmodifiableSet());
            reducibleLoopBodies.forEach(loopBody -> collapse(loopBody, collapsibleDJGraph));
            allReducibleLoopBodies.addAll(reducibleLoopBodies);
            int x = treeDepth;
            Set<Set<V>> irreducibleLoopBodies = crossJoinEdges.stream().flatMap(cje -> {
                Set<V> nodesAtOrGreaterDepth = depthToNodeMap.entrySet().stream().filter(entry -> entry.getKey() >= x).flatMap(entry -> entry.getValue().stream()).collect(Collectors.toUnmodifiableSet());
                Graph<V, E> inducedSubgraph = new AsSubgraph<>(collapsibleDJGraph, nodesAtOrGreaterDepth);
                List<Graph<V, E>> stronglyConnectedComponents = stronglyConnectedComponents(inducedSubgraph);
                stronglyConnectedComponents.forEach(scc -> collapse(scc, collapsibleDJGraph));
                return stronglyConnectedComponents.stream().map(scc -> scc.vertexSet());
            }).collect(Collectors.toUnmodifiableSet());
            allIrreducibleLoopBodies.addAll(irreducibleLoopBodies);
        }
        return ImmutablePair.of(allReducibleLoopBodies, allIrreducibleLoopBodies);
    }

    private void collapse(Set<V> reducibleLoopBody, Graph<V, E> collapsibleDJGraph) {

    }

    private void collapse(Graph<V, E> scc, Graph<V, E> collapsibleDJGraph) {

    }

    private static <V extends Identifiable, E> List<Graph<V, E>> stronglyConnectedComponents(Graph<V, E> inducedSubgraph) {
        StrongConnectivityAlgorithm<V, E> scAlg = new KosarajuStrongConnectivityInspector<>(inducedSubgraph);
        return scAlg.getStronglyConnectedComponents();
    }

    private Graph<V, E> clone(Graph<V, E> djGraph) {
        Graph<V, E> clonedGraph = new DefaultDirectedGraph<>(edgeClass);
        djGraph.vertexSet().forEach(clonedGraph::addVertex);
        djGraph.edgeSet().forEach(edge -> clonedGraph.addEdge(djGraph.getEdgeSource(edge), djGraph.getEdgeTarget(edge), edge));
        return clonedGraph;
    }
}
