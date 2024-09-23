package org.smojol.common.graph;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;

import java.util.*;
import java.util.stream.Collectors;

import static org.smojol.common.list.ConsCar.tail;

public class DepthFirstTraversalLabelTask {
    public static final String DFS_NUM = "DFS_NUM";
    private final List<GraphNodeLike> depthFirstSpanningTreeOrder = new ArrayList<>();
    private final GraphNodeLike root;
    private final Graph<GraphNodeLike, DefaultEdge> graph;
    private int currentDfsNumber;

    public DepthFirstTraversalLabelTask(GraphNodeLike root, Graph<GraphNodeLike, DefaultEdge> graph) {
        this(root, graph, 0);
    }

    public DepthFirstTraversalLabelTask(GraphNodeLike root, Graph<GraphNodeLike, DefaultEdge> graph, int startNumber) {
        this.root = root;
        this.graph = graph;
        this.currentDfsNumber = startNumber;
    }

    public void run() {
        run(root);
    }

    public void run(GraphNodeLike current) {
        current.setProperty(DFS_NUM, currentDfsNumber);
        depthFirstSpanningTreeOrder.add(current);
        currentDfsNumber++;
        List<GraphNodeLike> unvisitedChildren = graph.outgoingEdgesOf(current).stream()
                .map(graph::getEdgeTarget).toList();
        for (GraphNodeLike child : unvisitedChildren) {
            if (child.getProperty(DFS_NUM, Integer.class) != null) continue;
            run(child);
        }
    }

    public int max() {
        return currentDfsNumber;
    }

    public List<GraphNodeLike> preOrder() {
        return depthFirstSpanningTreeOrder;
    }

    public List<GraphNodeLike> postOrder() {
        return depthFirstSpanningTreeOrder.reversed();
    }

    public Map<GraphNodeLike, Set<GraphNodeLike>> allDominators() {
        run(root);
        List<GraphNodeLike> order = preOrder();
        Set<GraphNodeLike> allNodes = new HashSet<>(order);
        Map<GraphNodeLike, Set<GraphNodeLike>> dominators = new HashMap<>();
        dominators.put(order.getFirst(), Set.of(order.getFirst()));
        List<GraphNodeLike> tail = tail(order);
        tail.forEach(node -> dominators.put(node, new HashSet<>(order)));

        do {
        } while (tail.stream().map(n -> {
            Set<GraphNodeLike> predecessors = graph.incomingEdgesOf(n).stream().map(graph::getEdgeSource).collect(Collectors.toUnmodifiableSet());
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

    public List<ImmutablePair<GraphNodeLike, GraphNodeLike>> immediateDominators() {
        Map<GraphNodeLike, Set<GraphNodeLike>> allDominators = allDominators();
        List<Pair<GraphNodeLike, GraphNodeLike>> dominances = allDominators.entrySet().stream().flatMap(e -> e.getValue().stream().map(dom -> (Pair<GraphNodeLike, GraphNodeLike>) ImmutablePair.of(dom, e.getKey()))).toList();
        return allDominators.entrySet().stream().map(e -> ImmutablePair.of(e.getKey(), uniqueImmediateDominator(e.getKey(), e.getValue(), dominances))).toList();
    }

    private GraphNodeLike uniqueImmediateDominator(GraphNodeLike dominated, Set<GraphNodeLike> potentialImmediateDominators, List<Pair<GraphNodeLike, GraphNodeLike>> dominances) {
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
