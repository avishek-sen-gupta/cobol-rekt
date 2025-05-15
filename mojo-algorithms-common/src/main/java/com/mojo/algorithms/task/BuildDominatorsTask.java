package com.mojo.algorithms.task;

import com.google.common.collect.Sets;
import com.mojo.algorithms.domain.DepthFirstSpanningTree;
import com.mojo.algorithms.id.Identifiable;
import com.mojo.algorithms.list.CarCdr;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;

import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;


/*
Algorithm based on the paper 'Graph-Theoretic Constructs for Program Control Flow Analysis' by Allen and Cocke (1972)
 */
public class BuildDominatorsTask<V extends Identifiable, E> {
    private static final Logger LOGGER = Logger.getLogger(BuildDominatorsTask.class.getName());

    public Map<V, Set<V>> allDominators(List<V> dfsOrdered, Graph<V, E> sourceGraph) {
        Set<V> allNodes = new HashSet<>(dfsOrdered);
        Map<V, Set<V>> dominators = new HashMap<>();
        dominators.put(dfsOrdered.getFirst(), Set.of(dfsOrdered.getFirst()));
        List<V> tail = CarCdr.tail(dfsOrdered);
        tail.forEach(node -> dominators.put(node, new HashSet<>(dfsOrdered)));

        do {
            LOGGER.finer("Building Dominators...");
        } while (tail.stream().map(n -> {
            Set<V> predecessors = sourceGraph.incomingEdgesOf(n).stream().map(sourceGraph::getEdgeSource).collect(Collectors.toUnmodifiableSet());
            List<Set<V>> predecessorDominators = predecessors.stream().map(dominators::get).toList();
            Set<V> finalIntersection = predecessorDominators.stream().reduce(allNodes, Sets::intersection);
            Set<V> updatedDominatorSet = Sets.union(finalIntersection, Set.of(n));
            Set<V> originalDominatorSet = dominators.get(n);
            LOGGER.finest(String.format("Comparing %s with %s", updatedDominatorSet, originalDominatorSet));
            if (updatedDominatorSet.equals(originalDominatorSet)) {
                LOGGER.finest("Were not equal, returning false");
                return false;
            }
            dominators.put(n, updatedDominatorSet);
            LOGGER.finest("Were equal, returning true");
            return true;
        }).reduce(false, (a, b) -> a || b));

//        dominators.put(dfsOrdered.getFirst(), ImmutableSet.of());
        return dominators;
    }

    public List<Pair<V, V>> immediateDominators(DepthFirstSpanningTree<V, E> dfsTree) {
        Map<V, Set<V>> allDominators = allDominators(dfsTree.preOrdered(), dfsTree.sourceGraph());
        LOGGER.info("Building Immediate Dominators >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
        Map<V, List<V>> dominances = new HashMap<>();
        allDominators.forEach((dominated, dominators) -> dominators.forEach(dominator -> {
            if (!dominances.containsKey(dominator)) dominances.put(dominator, new ArrayList<>());
            dominances.get(dominator).add(dominated);
        }));
        return allDominators.entrySet().stream().map(e -> (Pair<V, V>) ImmutablePair.of(e.getKey(), uniqueImmediateDominator(e.getKey(), e.getValue(), dfsTree.sourceGraphRoot(), dominances))).toList();
    }

    private V uniqueImmediateDominator(V dominated, Set<V> potentialImmediateDominators, V root, Map<V, List<V>> allDominances) {
        if (dominated == root) return null;
        Set<V> potentialImmediateDominatorsWithoutSelf = new HashSet<>(potentialImmediateDominators);
        List<V> nonImmediateDominatorNodes = new ArrayList<>();
        potentialImmediateDominatorsWithoutSelf.remove(dominated);
        for (V d1 : potentialImmediateDominatorsWithoutSelf) {
            for (V d2 : potentialImmediateDominatorsWithoutSelf) {
                if (d1 == d2) continue;
                if (allDominances.get(d1).contains(d2)) nonImmediateDominatorNodes.add(d1);
            }
        }

        nonImmediateDominatorNodes.forEach(potentialImmediateDominatorsWithoutSelf::remove);
        return potentialImmediateDominatorsWithoutSelf.stream().findFirst().get();
    }
}
