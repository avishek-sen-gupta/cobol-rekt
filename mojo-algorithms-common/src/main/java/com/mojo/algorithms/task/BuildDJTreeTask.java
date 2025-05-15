package com.mojo.algorithms.task;

import com.google.common.collect.Sets;
import com.mojo.algorithms.domain.DJTree;
import com.mojo.algorithms.domain.DepthFirstSpanningTree;
import com.mojo.algorithms.domain.DominatorTree;
import com.mojo.algorithms.id.Identifiable;
import com.mojo.algorithms.domain.BackJoinEdge;
import com.mojo.algorithms.domain.CrossJoinEdge;
import com.mojo.algorithms.domain.DominatorEdge;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/*
Algorithm based on the paper 'A Linear Time Algorithm for Placing Phi-Nodes' by Sreedhar (1996)
 */
public class BuildDJTreeTask<V extends Identifiable, E> {
    private static final Logger LOGGER = Logger.getLogger(BuildDJTreeTask.class.getName());
    private final DominatorTree<V, E> dominatorTree;
    private final DepthFirstSpanningTree<V, E> spanningTree;
    private final Map<V, Set<V>> allDominators;
    private final Class<E> edgeClass;

    public BuildDJTreeTask(DominatorTree<V, E> dominatorTree, DepthFirstSpanningTree<V, E> spanningTree, Map<V, Set<V>> allDominators, Class<E> edgeClass) {
        this.dominatorTree = dominatorTree;
        this.spanningTree = spanningTree;
        this.allDominators = allDominators;
        this.edgeClass = edgeClass;
    }

    public DJTree<V, E> run() {
        Graph<V, E> sourceGraph = spanningTree.sourceGraph();
        Graph<V, DefaultEdge> djTree = new DefaultDirectedGraph<>(DefaultEdge.class);
        Graph<V, E> dominatorGraph = dominatorTree.graph();
        sourceGraph.vertexSet().forEach(djTree::addVertex);
        dominatorGraph.edgeSet().forEach(edge -> djTree.addEdge(dominatorGraph.getEdgeSource(edge), dominatorGraph.getEdgeTarget(edge), new DominatorEdge()));
        V root = dominatorTree.root();
        DepthFirstSpanningTree<V, E> dominatorSpanningTree = new DepthFirstSearchOrderingTask<>(root, dominatorGraph, edgeClass).run();
        Sets.difference(sourceGraph.edgeSet(), dominatorGraph.edgeSet()).forEach(edge -> {
            V from = sourceGraph.getEdgeSource(edge);
            V to = sourceGraph.getEdgeTarget(edge);
            if (allDominators.get(from).contains(to))
                djTree.addEdge(from, to, new BackJoinEdge());
            else djTree.addEdge(from, to, new CrossJoinEdge());
        });
        Set<Integer> dominatorLevels = dominatorSpanningTree.nodeStats().values().stream().map(NodeDFSStatistics::treeDepth).collect(Collectors.toUnmodifiableSet());
        Map<Integer, Set<V>> dominatorLevelMap = dominatorLevels.stream().collect(Collectors.toMap(i -> i, i -> new HashSet<>()));
        dominatorSpanningTree.nodeStats().forEach((key, value) -> {
            int level = value.treeDepth();
            dominatorLevelMap.get(level).add(key);
        });
        // Trust me bro
        return new DJTree<>(spanningTree.sourceGraphRoot(), (Graph<V, E>) djTree, dominatorLevelMap);
    }
}
