package com.mojo.algorithms.task;

import com.google.common.collect.Streams;
import com.mojo.algorithms.domain.GraphSlice;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.graph.AsSubgraph;
import org.jgrapht.graph.GraphWalk;
import com.mojo.algorithms.id.Identifiable;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GraphSliceTask<V extends Identifiable, E> {
    private final List<V> visited = new ArrayList<>();
    private final Graph<V, E> sourceGraph;
    private final Class<E> edgeClass;
    private final List<GraphPath<V, E>> allPaths = new ArrayList<>();

    public GraphSliceTask(Graph<V, E> sourceGraph, Class<E> edgeClass) {
        this.sourceGraph = sourceGraph;
        this.edgeClass = edgeClass;
    }

    public GraphSlice<V, E> run(V source, V sink) {
        List<Pair<V, E>> stack = new ArrayList<>();
        run(source, null, source, sink, stack);
        Set<V> graphSliceVertices = allPaths.stream().flatMap(path -> path.getVertexList().stream()).collect(Collectors.toUnmodifiableSet());
        Set<E> graphSliceEdges = allPaths.stream().flatMap(path -> path.getEdgeList().stream()).collect(Collectors.toUnmodifiableSet());
        Graph<V, E> inducedSubgraph = new AsSubgraph<>(sourceGraph, graphSliceVertices);
        List<V> toplogicallyOrderedVertices = new DepthFirstSearchOrderingTask<>(source, inducedSubgraph, edgeClass).run().topologicallyOrdered();
        return new GraphSlice<>(allPaths, toplogicallyOrderedVertices, sourceGraph);
    }

    private void run(V current, E incomingEdge, V source, V sink, List<Pair<V, E>> dfsStack) {
        if (visited.contains(current)) {
            if (!alreadyInPath(current, dfsStack)) return;
            setupFinalPath(source, current, incomingEdge, dfsStack);
            return;
        } else if (current == sink) {
            visited.add(current);
            setupFinalPath(source, current, incomingEdge, Streams.concat(dfsStack.stream(), Stream.of(ImmutablePair.of(current, incomingEdge))).toList());
            return;
        }
        visited.add(current);
        sourceGraph.outgoingEdgesOf(current).forEach(e -> {
            run(sourceGraph.getEdgeTarget(e), e, source, sink,
                    incomingEdge == null ? new ArrayList<>(dfsStack) : Streams.concat(dfsStack.stream(), Stream.of(ImmutablePair.of(current, incomingEdge))).collect(Collectors.toList()));
        });
    }

    private void setupFinalPath(V source, V current, E incomingEdge, List<Pair<V, E>> stack) {
//        stack.add(ImmutablePair.of(current, incomingEdge));
        allPaths.add(new GraphWalk<>(sourceGraph, source, current,
                Streams.concat(Stream.of(source), stack.stream().map(Pair::getLeft)).toList(),
                stack.stream().map(Pair::getRight).toList(), 1));
    }

    private boolean alreadyInPath(V current, List<Pair<V, E>> dfsStack) {
        return allPaths.stream().anyMatch(path -> path.getVertexList().contains(current))
                && dfsStack.stream().noneMatch(p -> p.getLeft().equals(current));
    }
}
