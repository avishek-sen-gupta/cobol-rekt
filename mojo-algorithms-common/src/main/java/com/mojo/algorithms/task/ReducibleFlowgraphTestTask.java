package com.mojo.algorithms.task;

import com.mojo.algorithms.domain.ClassifiedEdges;
import com.mojo.algorithms.domain.DJTree;
import com.mojo.algorithms.domain.DepthFirstSpanningTree;
import com.mojo.algorithms.domain.DominatorTree;
import com.mojo.algorithms.id.Identifiable;
import com.mojo.algorithms.domain.CrossJoinEdge;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class ReducibleFlowgraphTestTask<V extends Identifiable, E> {

    private final V sourceGraphRoot;
    private final Graph<V, E> sourceGraph;
    private final Class<E> edgeClass;

    public ReducibleFlowgraphTestTask(V sourceGraphRoot, Graph<V, E> sourceGraph, Class<E> edgeClass) {
        this.sourceGraphRoot = sourceGraphRoot;
        this.sourceGraph = sourceGraph;
        this.edgeClass = edgeClass;
    }

    public boolean run() {
        DepthFirstSearchOrderingTask<V, E> dfsTask = new DepthFirstSearchOrderingTask<>(sourceGraphRoot, sourceGraph, edgeClass);
        DepthFirstSpanningTree<V, E> spanningTree = dfsTask.run();

        List<Pair<V, V>> immediateDominators = new BuildDominatorsTask<V, E>().immediateDominators(spanningTree);
        Map<V, Set<V>> allDominators = new BuildDominatorsTask<V, E>().allDominators(spanningTree.preOrdered(), sourceGraph);
        DominatorTree<V, E> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), edgeClass).run();
        DJTree<V, E> djTree = new BuildDJTreeTask<>(dominatorTree, spanningTree, allDominators, edgeClass).run();
        DepthFirstSearchOrderingTask<V, E> dfsTaskOnDJTree = new DepthFirstSearchOrderingTask<>(djTree.root(), djTree.graph(), edgeClass);
        DepthFirstSpanningTree<V, E> djSpanningTree = dfsTaskOnDJTree.run();
        ClassifiedEdges<E> classifiedEdges = djSpanningTree.classifiedEdges();
        Set<E> backEdges = classifiedEdges.backEdges();
        return backEdges.stream().noneMatch(be -> be.getClass() == CrossJoinEdge.class);
    }
}
