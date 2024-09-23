package org.smojol.common.graph;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;

import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import static org.smojol.common.list.ConsCar.tail;

public class BuildDominatorsTask {
    private static final Logger LOGGER = Logger.getLogger(BuildDominatorsTask.class.getName());

    public Map<GraphNodeLike, Set<GraphNodeLike>> allDominators(List<GraphNodeLike> dfsOrdered, Graph<GraphNodeLike, DefaultEdge> g) {
        Set<GraphNodeLike> allNodes = new HashSet<>(dfsOrdered);
        Map<GraphNodeLike, Set<GraphNodeLike>> dominators = new HashMap<>();
        dominators.put(dfsOrdered.getFirst(), Set.of(dfsOrdered.getFirst()));
        List<GraphNodeLike> tail = tail(dfsOrdered);
        tail.forEach(node -> dominators.put(node, new HashSet<>(dfsOrdered)));

        do {
            LOGGER.finer("Building Dominators...");
        } while (tail.stream().map(n -> {
            Set<GraphNodeLike> predecessors = g.incomingEdgesOf(n).stream().map(g::getEdgeSource).collect(Collectors.toUnmodifiableSet());
            Set<GraphNodeLike> dominatorSet = new HashSet<>();
            dominatorSet.add(n);
            List<Set<GraphNodeLike>> predDominators = predecessors.stream().map(dominators::get).toList();
            Set<GraphNodeLike> finalIntersection = predDominators.stream().reduce(allNodes, (a, b) -> {
                HashSet<GraphNodeLike> intersection = new HashSet<>(a);
                intersection.retainAll(b);
                return intersection;
            });
            dominatorSet.addAll(finalIntersection);
            if (dominatorSet.equals(dominators.get(n))) {
                dominators.put(n, dominatorSet);
                return false;
            }
            dominators.put(n, dominatorSet);
            return true;
        }).reduce(false, (a, b) -> a || b));

        return dominators;
    }

    public List<ImmutablePair<GraphNodeLike, GraphNodeLike>> immediateDominators(List<GraphNodeLike> dfsOrdered, Graph<GraphNodeLike, DefaultEdge> g, GraphNodeLike root) {
        Map<GraphNodeLike, Set<GraphNodeLike>> allDominators = allDominators(dfsOrdered, g);
        List<Pair<GraphNodeLike, GraphNodeLike>> dominances = allDominators.entrySet().stream().flatMap(e -> e.getValue().stream().map(dom -> (Pair<GraphNodeLike, GraphNodeLike>) ImmutablePair.of(dom, e.getKey()))).toList();
        return allDominators.entrySet().stream().map(e -> ImmutablePair.of(e.getKey(), uniqueImmediateDominator(e.getKey(), e.getValue(), dominances, root))).toList();
    }

    private GraphNodeLike uniqueImmediateDominator(GraphNodeLike dominated, Set<GraphNodeLike> potentialImmediateDominators, List<Pair<GraphNodeLike, GraphNodeLike>> dominances, GraphNodeLike root) {
        if (dominated == root) return dominated;
        HashSet<GraphNodeLike> potentialImmediateDominatorsWithoutSelf = new HashSet<>(potentialImmediateDominators);
        List<GraphNodeLike> removals = new ArrayList<>();
        potentialImmediateDominatorsWithoutSelf.remove(dominated);
        for (GraphNodeLike d1 : potentialImmediateDominatorsWithoutSelf) {
            for (GraphNodeLike d2 : potentialImmediateDominatorsWithoutSelf) {
                if (d1 == d2) continue;
                if (dominances.contains(ImmutablePair.of(d1, d2))) removals.add(d1);
            }
        }

        removals.forEach(potentialImmediateDominatorsWithoutSelf::remove);
        return potentialImmediateDominatorsWithoutSelf.stream().findFirst().get();
    }
}
