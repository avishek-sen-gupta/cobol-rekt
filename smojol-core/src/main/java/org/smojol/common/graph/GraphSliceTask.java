package org.smojol.common.graph;

import com.google.common.collect.Streams;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.graph.GraphWalk;
import org.smojol.common.id.Identifiable;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GraphSliceTask<V extends Identifiable, E> {
    private final List<V> visited = new ArrayList<>();
    private final Graph<V, E> sourceGraph;
    private final List<GraphPath<V, E>> allPaths = new ArrayList<>();

    public GraphSliceTask(Graph<V, E> sourceGraph) {
        this.sourceGraph = sourceGraph;
    }

    public GraphSlice<V, E> run(V source, V sink) {
        List<Pair<V, E>> stack = new ArrayList<>();
        run(source, source, null, sink, stack);
        return new GraphSlice<>(allPaths);
    }

    public void run(V source, V current, E incomingEdge, V sink, List<Pair<V, E>> stack) {
        if (current == sink) {
            setupFinalPath(source, current, incomingEdge, sink, (List<Pair<V, E>>) stack);
            return;
        } else if (visited.contains(current)) {
            if (!alreadyInPath(current)) return;
            setupFinalPath(source, current, incomingEdge, sink, (List<Pair<V, E>>) stack);
            return;
        }
        visited.add(current);
        sourceGraph.outgoingEdgesOf(current).forEach(e -> {
            run(source, sourceGraph.getEdgeTarget(e), e, sink,
                    incomingEdge == null ? new ArrayList<>(stack) : Streams.concat(stack.stream(), Stream.of(ImmutablePair.of(current, incomingEdge))).collect(Collectors.toList()));
        });
    }

    private void setupFinalPath(V source, V current, E incomingEdge, V sink, List<Pair<V, E>> stack) {
        stack.add(ImmutablePair.of(current, incomingEdge));
        allPaths.add(new GraphWalk<>(sourceGraph, source, sink,
                Streams.concat(Stream.of(source), stack.stream().map(Pair::getLeft)).toList(),
                stack.stream().map(Pair::getRight).toList(), 1));
    }

    private boolean alreadyInPath(V current) {
        return allPaths.stream().anyMatch(path -> path.getVertexList().contains(current));
    }
}
