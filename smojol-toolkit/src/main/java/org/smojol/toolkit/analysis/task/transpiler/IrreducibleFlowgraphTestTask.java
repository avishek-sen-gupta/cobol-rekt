package org.smojol.toolkit.analysis.task.transpiler;

import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.smojol.common.graph.*;
import org.smojol.common.id.Identifiable;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class IrreducibleFlowgraphTestTask<V extends Identifiable, E> {

    private final V sourceGraphRoot;
    private final Graph<V, E> sourceGraph;
    private final Class<E> edgeClass;

    public IrreducibleFlowgraphTestTask(V sourceGraphRoot, Graph<V, E> sourceGraph, Class<E> edgeClass) {
        this.sourceGraphRoot = sourceGraphRoot;
        this.sourceGraph = sourceGraph;
        this.edgeClass = edgeClass;
    }

    public boolean run() {
        DepthFirstTraversalLabelTask<V, E> dfsTask = new DepthFirstTraversalLabelTask<>(sourceGraphRoot, sourceGraph, edgeClass);
        DepthFirstSpanningTree<V, E> spanningTree = dfsTask.run();

        List<Pair<V, V>> immediateDominators = new BuildDominatorsTask<V, E>().immediateDominators(spanningTree);
        Map<V, Set<V>> allDominators = new BuildDominatorsTask<V, E>().allDominators(spanningTree.preOrder(), sourceGraph);
        DominatorTree<V, E> dominatorTree = new BuildDominatorTreeTask<>(immediateDominators, spanningTree.sourceGraphRoot(), edgeClass).run();
        DJTree<V, E> djTree = new BuildDJTreeTask<>(dominatorTree, spanningTree, allDominators).run();
        DepthFirstTraversalLabelTask<V, E> dfsTaskOnDJTree = new DepthFirstTraversalLabelTask<>(djTree.root(), djTree.graph(), edgeClass);
        DepthFirstSpanningTree<V, E> djSpanningTree = dfsTaskOnDJTree.run();
        ClassifiedEdges<E> classifiedEdges = djSpanningTree.classifiedEdges();
        Set<E> backEdges = classifiedEdges.backEdges();
        return backEdges.stream().anyMatch(be -> be.getClass() == BackJoinEdge.class);
    }
}
